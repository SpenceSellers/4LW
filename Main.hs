module Main where
import Machine
import Control.Monad.State.Lazy
import Base27 (maxWord)
main :: IO ()
main = do
  putStrLn $ show $ runState tick blankState
