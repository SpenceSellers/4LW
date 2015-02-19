module Main where
import Machine
import Control.Monad.State.Lazy
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
