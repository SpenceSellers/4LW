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
  putStrLn $ show $ (_registers state)
  putStrLn $ exportString (_memory state) (minWord, wrd "__AA")
  putStrLn $ show $ range (minWord, wrd "__AA")
  putStrLn $ show $ parseOperands [wrd "___R", wrd "___B", wrd "___M", wrd "ABCD"]
  let state' = memory %~ importString "ADXZ___R___B___R___A___MABCD" (wrd "____") $ state
  putStrLn $ exportString (_memory state') (minWord, wrd "_AAA")
  let Right (InstructionParseResult raw num) = parseInstruction (wrd "____") (_memory state')
      ins = constructInstruction raw
  putStrLn $ show $ raw
  putStrLn $ show $ ins
            
  
  
