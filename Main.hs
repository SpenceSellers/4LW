module Main where
import Machine
import Control.Monad.State.Lazy
main :: IO ()
main = do
  putStrLn $ show $ runState tick blankState
