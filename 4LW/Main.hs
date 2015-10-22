module Main where
import Machine
import Control.Monad.State.Lazy
import Control.Lens
import Data.Maybe
import Data.Ix
import Base27
import Memory
import Instruction
import Registers
import System.Environment

sanitizeProg :: [Char] -> [Char]
sanitizeProg = filter Base27.isLetter

main :: IO ()
main = do
  --let (_, state) = runState tick blankState
  args <- getArgs
  let filename = head args
  prog <- readFile filename

  let state = memory %~ fromJust . importString (sanitizeProg prog) minWord $ blankState
  (_, state') <- runStateT (start 1000) state
  putStrLn "\n\n\n\n\n\n"
  putStrLn "Done:"
  putStrLn $ exportString (_memory state') (minWord, wrd "_AAA")

  putStrLn "Registers:"
  print $ state' ^. registers

  putStr "PC was at: "
  print $ fromJust $ state' ^? registers . ix pcRegister

  putStr "Ticks: "
  print $ state' ^. tickNum
