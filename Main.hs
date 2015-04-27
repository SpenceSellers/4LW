
module Main where
import Machine
import Control.Monad.State.Lazy
import Control.Lens
import Data.Ix
import Base27
import Memory
import Instruction
import System.Environment
    
main :: IO ()
main = do
  --let (_, state) = runState tick blankState
  args <- getArgs
  let filename = head args
  prog <- readFile filename
  let state = blankState
  let state' = memory %~ importString
               prog
               (wrd "____") $ state
  (_, state'') <- runStateT run state'
  putStrLn $ exportString (_memory state'') (minWord, wrd "_AAA")
  putStrLn $ show $ state'' ^. registers
