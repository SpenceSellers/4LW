module Main where
import Machine
import Control.Monad.State.Lazy
import Control.Lens
import Data.Maybe
import Data.Ix
import Base27
import Memory
import Instruction
import System.Environment

sanitizeProg :: [Char] -> [Char]
sanitizeProg = filter Base27.isLetter

main :: IO ()
main = do
  --let (_, state) = runState tick blankState
  args <- getArgs
  let filename = head args
  prog <- readFile filename

  let state = memory %~ fromJust . importString (sanitizeProg prog) (wrd "____") $ blankState
  (_, state') <- runStateT start state
  putStrLn "\n\n\n\n\n\n"
  putStrLn "Done:"
  putStrLn $ exportString (_memory state') (minWord, wrd "_AAA")
  print $ state' ^. registers
