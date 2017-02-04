-- Sometimes the user may want to do things outside the sandbox of the VM,
-- like inspecting memory, decoding instructions, or even just exiting 4LW.
-- That interface is designed here.


module VMInterface where
import qualified Machine as M
import qualified Instruction
import qualified Memory
import qualified Base27
import qualified Io
import qualified Stacks



import Control.Lens
import Control.Monad
import Data.Maybe
import Control.Monad.State.Lazy
import Control.Applicative
import Control.Monad.Reader
import Data.List.Split
import System.IO
import Data.Array
import Data.List

maybeRead = fmap fst . listToMaybe . reads

-- | Brings a function into the State monad.
hoistState :: Monad m => State s a -> StateT s m a
hoistState = StateT . (return .) . runState

-- |Sets up and runs the VM interface.
interface :: StateT M.MachineState IO ()
interface = do
    liftIO $ Io.unprepareTerminal
    lift $ putStrLn ""
    lift $ putStrLn "====== VM Control ======"
    pc <- hoistState M.getPC
    lift . putStrLn $ "PC: " ++ show pc
    commandLoop
    lift $ putStrLn "====== Resume VM ======"
    liftIO $ Io.prepareTerminal

parseWord :: String -> Maybe Base27.Word
parseWord s = case Base27.toWord <$> maybeRead s of
    Just w -> Just w
    Nothing -> Base27.wrdSafe s

parseLetter :: String -> Maybe Base27.Letter
parseLetter s = Base27.letterSafe =<< s ^? ix 0

-- |Often the Program Counter acts as a good default for an address, if none other is provided.
elsePC :: Maybe Base27.Word -> StateT M.MachineState IO Base27.Word
elsePC a = fromMaybe <$> hoistState M.getPC <*> pure a

-- |Defines and describes a VM Interface command.
data Command = Command
    { name :: String
    , description:: String
    , action :: [String] -> StateT M.MachineState IO ()
    , continue :: Bool
    }

-- |Provides a pretty string that describes a command
commandHelp :: Command -> String 
commandHelp cmd = name cmd ++ ": " ++ description cmd

unknownCommand = Command 
    { name = "unknown"
    , description = "Internal command run on unknown command"
    , action = \_ -> liftIO . putStrLn $ "Unknown command!"
    , continue = True
    }

resumeCmd = Command 
    { name = "resume"
    , description = "Resumes the VM"
    , action = \_ -> return ()
    , continue = False
    }

stopCmd = Command 
    { name = "stop"
    , description = "Powers off the VM"
    , action = \_ -> M.action .= M.HaltAction
    , continue = False
    }

helpCmd = Command 
    { name = "help"
    , description = "Prints help info"
    , action = \_ -> liftIO . sequence_ . fmap (putStrLn . commandHelp) $ commands
    , continue = True
    }

peekCmd = Command 
    { name = "peek"
    , description = "Looks at a memory address"
    , action = \(addrString: _) -> do
        let addr = fromJust $ parseWord addrString
        mem <- use M.memory
        let word = Memory.readWord mem addr
        lift $ putStrLn (show word)

    , continue = True
    }

pokeCmd = Command 
    { name = "poke"
    , description = "Puts a value into a memory address"
    , action = \(addrString : valueString : _) -> do
        let addr = fromJust $ parseWord addrString
        let val = fromJust $ parseWord valueString
        hoistState $ M.setMemory addr val

    , continue = True
    }

decodeCmd = Command 
    { name = "d"
    , description = "Decodes an instruction"
    , action = \args -> do
        addr <- elsePC (parseWord =<< (args ^? ix 0))
        mem <- use M.memory
        let result = Instruction.readInstruction addr mem
        case result of
            Right (Instruction.InstructionParseResult ins len) -> do
                lift . putStrLn . show $ ins
                lift . putStrLn $ "Length: " ++ (show len)
                lift . putStrLn $ "Addr: " ++ show addr
            Left err -> lift . putStrLn $ "Invalid instruction: " ++ (show err)
    , continue = True
    }
        

decodeSequenceCmd = Command 
    { name = "ds"
    , description = "Decodes a sequence of instructions"
    , action = \args -> do -- Decode Sequence
        addr <- elsePC (parseWord =<< (args ^? ix 0))
        mem <- use M.memory
        let inss = Instruction.readSequence mem addr
        mapM_ (\(ins, insaddr) -> lift . putStrLn $ (show insaddr) ++ ": " ++ (show ins)) inss

    , continue = True
    }

regsCmd = Command 
    { name = "regs"
    , description = "Dumps registers"
    , action = \_ -> dumpRegs
    , continue = True
    }

stackCmd = Command 
    { name = "stack"
    , description = "Prints a stack"
    , action = \args -> do
        let stackid = fromJust $ parseLetter (args !! 0)
        stacks_ <- use M.stacks
        let vals = Stacks.peekAll (stacks_ ! stackid)
        lift . putStrLn $ (show (length vals)) ++ " items on stack " ++ show stackid
        lift . putStrLn $  ""
        mapM_ (lift . putStrLn . show) vals
  
    , continue = True
    }

-- |The list of commands that the VMInterface knows about.
commands :: [Command]
commands = [
      resumeCmd
    , stopCmd
    , helpCmd
    , peekCmd
    , pokeCmd
    , decodeCmd
    , decodeSequenceCmd
    , regsCmd
    , stackCmd
    ]

-- |Recursively loops, allowing the user to enter and run multiple commands.
commandLoop :: StateT M.MachineState IO ()
commandLoop = do
    liftIO $ putStr "[Control] "
    liftIO $ hFlush stdout
    rawcmd <- liftIO $ getLine
    let cmdlist = splitOn " " rawcmd
    let (cmd : args) = cmdlist
    -- Search through the commands and find the one with the right name.
    -- If it isn't found, run unknownCommand instead.
    let command = fromMaybe unknownCommand $ find (\c -> name c == cmd) commands
    -- run the command.
    action command $ args
    -- Should we continue, or exit the interface?
    case continue command of 
        True -> commandLoop
        False -> return ()
   
-- |Prints the state of all of the registers.
dumpRegs ::  StateT M.MachineState IO ()
dumpRegs = do
    reglist <- assocs <$> use M.registers
    mapM_ (\(reg, val) -> lift . putStrLn $ show reg ++ " : " ++ show val) reglist
