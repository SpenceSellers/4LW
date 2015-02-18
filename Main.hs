module Main where
import Machine
import Control.Monad.State.Lazy
import Data.Ix
import Base27
import Memory
main :: IO ()
main = do
  let (_, state) = runState tick blankState
  putStrLn $ show $ (_registers state)
  putStrLn $ exportString (_memory state) (minWord, wrd "__AA")
  putStrLn $ show $ range (minWord, wrd "__AA")
