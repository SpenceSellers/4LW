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

maybeRead = fmap fst . listToMaybe . reads

-- | Brings a function into the State monad.
hoistState :: Monad m => State s a -> StateT s m a
hoistState = StateT . (return .) . runState

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

elsePC :: Maybe Base27.Word -> StateT M.MachineState IO Base27.Word
elsePC a = fromMaybe <$> hoistState M.getPC <*> pure a

commandLoop :: StateT M.MachineState IO ()
commandLoop = do
    liftIO $ putStr "[Control] "
    liftIO $ hFlush stdout
    rawcmd <- liftIO $ getLine
    let cmdlist = splitOn " " rawcmd
    let (cmd : args) = cmdlist
    case cmd of
        "resume" -> return ()
        "`" -> return ()
        "poweroff" -> do
            M.action .= M.HaltAction

        "peek" -> do
            let addr = fromJust $ parseWord (args !! 0)
            mem <- use M.memory
            let word = Memory.readWord mem addr
            lift $ putStrLn (show word)
            commandLoop

        "poke" -> do
            let addr = fromJust $ parseWord (args !! 0)
            let val = fromJust $ parseWord (args !! 1)
            hoistState $ M.setMemory addr val
            commandLoop

        "d" -> do -- Decode
            addr <- elsePC (parseWord =<< (args ^? ix 0))

            mem <- use M.memory
            let result = Instruction.readInstruction addr mem
            case result of
                Right (Instruction.InstructionParseResult ins len) -> do
                    lift . putStrLn . show $ ins
                    lift . putStrLn $ "Length: " ++ (show len)
                    lift . putStrLn $ "Addr: " ++ show addr
                Left err -> lift . putStrLn $ "Invalid instruction: " ++ (show err)
            commandLoop

        "ds" -> do -- Decode Sequence
            addr <- elsePC (parseWord =<< (args ^? ix 0))
            mem <- use M.memory
            let inss = Instruction.readSequence mem addr
            mapM_ (\(ins, insaddr) -> lift . putStrLn $ (show insaddr) ++ ": " ++ (show ins)) inss
            commandLoop

        "regs" -> dumpRegs >> commandLoop

        "stack" -> do
            let stackid = fromJust $ parseLetter (args !! 0)
            stacks_ <- use M.stacks
            let vals = Stacks.peekAll (stacks_ ! stackid)
            lift . putStrLn $ (show (length vals)) ++ " items on stack " ++ show stackid
            lift . putStrLn $  ""
            mapM_ (lift . putStrLn . show) vals
            commandLoop


        _ -> commandLoop

dumpRegs ::  StateT M.MachineState IO ()
dumpRegs = do
    reglist <- assocs <$> use M.registers
    mapM_ (\(reg, val) -> lift . putStrLn $ show reg ++ " : " ++ show val) reglist
