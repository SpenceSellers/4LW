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
  --let (_, state) = runState tick blankState
  let state = blankState
  let state' = memory %~ importString
               "ADX____C___A___C___B___R___DMVP____C___A___R___AMVP____C___D___R___BADX____R___A___R___B___M__CCRR_"
               (wrd "____") $ state
  (_, state'') <- runStateT run state'
  putStrLn $ exportString (_memory state'') (minWord, wrd "_AAA")
  putStrLn $ show $ state'' ^. registers
