module Main where
import Machine
import Control.Monad.State.Lazy
import Control.Lens
import Data.Ix
import Base27
import Memory
import Instruction
main :: IO ()
main = do
  let (_, state) = runState tick blankState
  let state' = memory %~ importString "ADXZ___R___B___R___A___MABCD" (wrd "____") $ state
  (_, state'') <- runStateT run state'
  putStrLn $ exportString (_memory state'') (minWord, wrd "_AAA")
  putStrLn $ show $ state'' ^. registers
  
  
