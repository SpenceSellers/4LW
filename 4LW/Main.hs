{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified WordSequence as WS
import qualified Tapes

import System.Environment
import Control.Applicative
import Options.Applicative
import Text.Read
import Control.Exception


sanitizeProg :: [Char] -> [Char]
sanitizeProg = filter Base27.isLetter

data ProgOptions = ProgOptions { filename :: String
                             , tickTime :: Int
                             --, tapeFiles :: [(Letter, String)]
                             , tapeFile :: Maybe String
                             } deriving (Show)

parseOptions :: Parser ProgOptions
parseOptions = ProgOptions
                   <$> strArgument (metavar "FILE")
                   <*> option auto (short 't' <> value 1000)
                   <*> optional (strOption (short 'T'))

optionsAndInfo :: ParserInfo ProgOptions
optionsAndInfo = info (helper <*> parseOptions)
    (fullDesc <> progDesc "4LW is a virtual machine implementing a base-27 architecture.")

main :: IO ()
main = do
  --let (_, state) = runState tick blankState
  options <- execParser optionsAndInfo
  prog <- readFile (filename options)

  tapeStr <- case tapeFile options of
            Just fname -> catch (readFile fname) (\(e :: SomeException) -> putStrLn "Error reading tape" >> return "")
            Nothing -> return ""

  let statemem = memory %~ fromJust . importString (sanitizeProg prog) minWord $ blankState
  let state = tapeDeck . at (letter 'A') .~ Just (Tapes.newTape (WS.readWordsFiltered tapeStr)) $ statemem
  let runOptions = RunOptions {
    _ticktime = tickTime options,
    _commandFn = liftIO $ putStrLn "Command fn!"
  }

  (_, state') <- runStateT (start runOptions) state

  case tapeFile options of
    Just fname -> do
         putStrLn $ "Saving " ++ fname
         Tapes.writeTapeToFile fname (fromJust (view (tapeDeck . at (letter 'A')) $ state'))
    Nothing -> return ()
  putStrLn "\n\n\n\n\n\n"
  putStrLn "Done:"
  putStrLn $ exportString (_memory state') (minWord, wrd "_AAA")

  putStrLn "Registers:"
  print $ state' ^. registers

  putStr "PC was at: "
  print $ fromJust $ state' ^? registers . ix pcRegister

  putStr "Ticks: "
  print $ state' ^. tickNum
