-- | Handles common operations on sequences of Words.
module WordSequence where
import Prelude hiding (Word)
import Base27

import Data.List.Split

import Debug.Trace

tr x = trace (show x) x

-- | Converts a string into a list of 4LW letters, or
-- returns Nothing if it contains characters that aren't valid 4LW letters.
readLetters :: String -> Maybe [Letter]
readLetters = sequence . map letterSafe

-- |Turns a list of letters into a list of words.
-- If the length of letters isn't divisible by four, then
-- toWords will pad the end of the last word out with _'s
toWords :: [Letter] -> [Word]
toWords =  map wordFromList' . chunksOf 4

-- |Turns a string into a list of words (maybe)
-- The end-of-string behavior is the same as toWords.
readWords :: String -> Maybe [Word]
readWords s = toWords <$> readLetters s

-- |Converts a list of words to a contiguous string,
-- worthy of being read-back by readWords.
wordsToString :: [Word] -> String
wordsToString = concatMap show

-- |Reads a file containing Letters
readFileLetters :: String -> IO (Maybe [Letter])
readFileLetters filename = readLetters . removeNewlines <$> readFile filename

-- |Reads a file of Letters, turning them into Words.
readFileWords :: String -> IO (Maybe [Word])
readFileWords filename = readWords . removeNewlines <$> readFile filename

-- |Removes newlines from a string. Newlines are often considered acceptable
-- even in 'pure' 4LW letter files.
removeNewlines :: String -> String
removeNewlines = filter (/= '\n')

-- |Reads a string, IGNORING any letters that aren't valid 4LW letters.
-- This always produces a result, but may have weird results if you feed it
-- the wrong string.
readLettersFiltered :: String -> [Letter]
readLettersFiltered = map letter . filter isLetter

-- |Same as readLettersFiltered, but
readWordsFiltered :: String -> [Word]
readWordsFiltered = toWords . readLettersFiltered
