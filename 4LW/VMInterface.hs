module VMInterface where
import qualified Machine as M
import qualified Instruction
import qualified Io


import Control.Lens
import Control.Monad
import Data.Maybe
import Control.Monad.State.Lazy
import Control.Applicative
import Control.Monad.Reader
import Data.List.Split
import System.IO

interface :: StateT M.MachineState IO ()
interface = do
    liftIO $ Io.unprepareTerminal
    lift $ putStrLn ""
    lift $ putStrLn "====== VM Control ======"
    commandLoop
    liftIO $ Io.prepareTerminal

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
