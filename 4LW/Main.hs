module Main where
import Machine
import Control.Monad.State.Lazy
import Control.Lens hiding (argument)
import Data.Maybe
import Data.Ix
import Base27
import Memory
import Instruction
import Registers
import System.Environment
import Control.Applicative
import Options.Applicative


sanitizeProg :: [Char] -> [Char]
sanitizeProg = filter Base27.isLetter

data RunOptions = RunOptions { filename :: String
                             , tickTime:: Int
                             } deriving (Show)

parseOptions :: Parser RunOptions
parseOptions = RunOptions
                   <$> strArgument (metavar "FILE")
                   <*> option auto (short 't' <> value 1000)

optionsAndInfo :: ParserInfo RunOptions
optionsAndInfo = info (helper <*> parseOptions)
    (fullDesc <> progDesc "4LW is a virtual machine implementing a base-27 architecture.")

main :: IO ()
main = do
  --let (_, state) = runState tick blankState
  options <- execParser optionsAndInfo

  prog <- readFile (filename options)

  let state = memory %~ fromJust . importString (sanitizeProg prog) minWord $ blankState
  (_, state') <- runStateT (start (tickTime options)) state
  putStrLn "\n\n\n\n\n\n"
  putStrLn "Done:"
  putStrLn $ exportString (_memory state') (minWord, wrd "_AAA")

  putStrLn "Registers:"
  print $ state' ^. registers

  putStr "PC was at: "
  print $ fromJust $ state' ^? registers . ix pcRegister

  putStr "Ticks: "
  print $ state' ^. tickNum
