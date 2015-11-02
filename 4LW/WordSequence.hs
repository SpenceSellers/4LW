module WordSequence where
import Prelude hiding (Word)
import Base27

import Data.List.Split

import Debug.Trace

tr x = trace (show x) x

readLetters :: String -> Maybe [Letter]
readLetters = sequence . map letterSafe

toWords :: [Letter] -> [Word]
toWords =  map wordFromList' . chunksOf 4

readWords :: String -> Maybe [Word]
readWords s = toWords <$> readLetters s

wordsToString :: [Word] -> String
wordsToString = concatMap show

readFileLetters :: String -> IO (Maybe [Letter])
readFileLetters filename = readLetters . removeNewlines <$> readFile filename

readFileWords :: String -> IO (Maybe [Word])
readFileWords filename = readWords . removeNewlines <$> readFile filename

removeNewlines :: String -> String
removeNewlines = filter (/= '\n')
