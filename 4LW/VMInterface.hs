module VMInterface where
import qualified Machine as M
import qualified Instruction
import qualified Memory
import qualified Base27
import qualified Io


import Control.Lens
import Control.Monad
import Data.Maybe
import Control.Monad.State.Lazy
import Control.Applicative
import Control.Monad.Reader
import Data.List.Split
import System.IO

maybeRead = fmap fst . listToMaybe . reads

interface :: StateT M.MachineState IO ()
interface = do
    liftIO $ Io.unprepareTerminal
    lift $ putStrLn ""
    lift $ putStrLn "====== VM Control ======"
    commandLoop
    liftIO $ Io.prepareTerminal

parseWord :: String -> Maybe Base27.Word
parseWord s = case Base27.toWord <$> maybeRead s of
    Just w -> Just w
    Nothing -> case Base27.wrdSafe s of
        Just w -> Just w
        Nothing -> Nothing

commandLoop :: StateT M.MachineState IO ()
commandLoop = do
    liftIO $ putStr "[Control] "
    liftIO $ hFlush stdout
    rawcmd <- liftIO $ getLine
    let cmdlist = splitOn " " rawcmd
    let (cmd : args) = cmdlist
    case cmd of
        "resume" -> return ()
        "poweroff" -> do
            M.action .= M.HaltAction
        "peek" -> do
            let addr = fromJust $ parseWord (args !! 0)
            mem <- use M.memory
            let word = Memory.readWord mem addr
            lift $ putStrLn (show word)
            commandLoop
        "d" -> do -- Decode
            let addr = fromJust $ parseWord (args !! 0)
            mem <- use M.memory
            let result = Instruction.readInstruction addr mem
            case result of
                Right (Instruction.InstructionParseResult ins len) -> do
                    lift . putStrLn . show $ ins
                    lift . putStrLn $ "Length: " ++ (show len)
                Left err -> lift . putStrLn $ "Invalid instruction: " ++ (show err)
            commandLoop

        "ds" -> do -- Decode Sequence
            let addr = fromJust $ parseWord (args !! 0)
            mem <- use M.memory
            let inss = Instruction.readSequence mem addr
            mapM_ (lift . putStrLn . show) inss
            commandLoop
