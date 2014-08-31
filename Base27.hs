module Base27 where

import Data.Char
import Data.Ix
import Data.Digits (digits, unDigits)

newtype Letter = Letter Char deriving (Show, Ord, Eq, Ix)
    
isLetter :: Char -> Bool
isLetter '_' = True
isLetter c = inRange ('A', 'Z') c


getValue :: Letter -> Int
getValue (Letter '_') = 0
getValue (Letter l) = (index ('A', 'Z') l) + 1

-- | Converts a integer (0,26) to a letter. Yes this is a partial function.
toLetter :: Int -> Letter
toLetter num
    | num == 0 = Letter '_'
    | num < 0 = error "toLetter must be positive"
    | num > 26 = error "toLetter must be 26 or below."
    | otherwise = Letter $ chr ((ord 'A') + num - 1)

convertBase :: Integral a => a -> a -> [a] -> [a]
convertBase from to = digits to . unDigits from


newtype Word = Word (Letter, Letter, Letter, Letter) deriving (Show, Eq, Ord, Ix)

minWord :: Word
minWord = Word (Letter '_', Letter '_', Letter '_', Letter '_')

maxWord = Word (Letter 'Z', Letter 'Z', Letter 'Z', Letter 'Z')

wordValues :: Int
wordValues = 27 ^ 4

wordToList :: Word -> [Letter]
wordToList (Word (a,b,c,d)) = [a,b,c,d]

wordFromList :: [Letter] -> Word
wordFromList (a:b:c:d:_) = Word (a,b,c,d)
wordFromList (a:b:c:_) = Word (Letter '_', a, b, c)
wordFromList (a:b:_) = Word (Letter '_', Letter '_', a, b)
wordFromList (a:_) = Word (Letter '_', Letter '_', Letter '_', a)
wordFromList (_) = minWord

wordValue :: Word -> Int
--wordValue word = read $ concat $ map (show) $ convertBase 27 10 (map getValue (wordToList word))
wordValue word = unDigits 27 $ map getValue (wordToList word)

toWord :: Int -> Word
toWord val = wordFromList $ map toLetter $ digits 27 (val `mod` wordValues)

addWord :: Word -> Word -> Word
addWord w1 w2 = toWord $ (wordValue w1) + (wordValue w2)
                
