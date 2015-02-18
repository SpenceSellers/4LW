module Base27 where

import Data.Char
import Data.Ix
import Data.Digits (digits, unDigits)
    
newtype Letter = Letter Char deriving (Ord, Eq)

instance Ix Letter where
    range (l1, l2) = map letterValue $ range (n1, n2)
        where
          n1 = getValue l1
          n2 = getValue l2

    index (l1, l2) l = (getValue l) - (getValue l1)

    inRange (l1, l2) l = (getValue l >= getValue l1) && (getValue l <= getValue l2)

instance Show Letter where
    show (Letter c) = "." ++ [c]
                      
-- | Better constructor, with checking.
letter :: Char -> Letter
letter c = if Base27.isLetter c then Letter c else error "Bad letter value!"

isLetter :: Char -> Bool
isLetter '_' = True
isLetter c = inRange ('A', 'Z') c

-- | Gets the numeric value of a Letter.
getValue :: Letter -> Int
getValue (Letter '_') = 0
getValue (Letter l) = (index ('A', 'Z') l) + 1

-- | Turns a numeric value into a Letter
letterValue :: Int -> Letter
letterValue 0 = letter '_'
letterValue n = letter $ range ('A', 'Z') !! (n - 1)

-- | Converts a integer (0,26) to a letter. Yes this is a partial function.
toLetter :: Int -> Letter
toLetter num
    | num == 0 = letter '_'
    | num < 0 = error "toLetter must be positive"
    | num > 26 = error "toLetter must be 26 or below."
    | otherwise = letter $ chr ((ord 'A') + num - 1)


convertBase :: Integral a => a -> a -> [a] -> [a]
convertBase from to = digits to . unDigits from

-- | A word is basically a tuple of four letters.
newtype Word = Word (Letter, Letter, Letter, Letter) deriving (Eq, Ord)

-- | The default Show is hard to read, let's just cram the letters together.
instance Show Word where
    show word =
        [ca, cb, cc, cd] where
            (Word (a, b, c, d)) = word
            (Letter ca) = a
            (Letter cb) = b
            (Letter cc) = c
            (Letter cd) = d

instance Ix Word where
    range (w1, w2) = map toWord $ range (n1, n2)
        where
          n1 = wordValue w1
          n2 = wordValue w2

    index (w1, w2) w = (wordValue w) - (wordValue w1)

    inRange (w1, w2) w = (wordValue w >= wordValue w1) && (wordValue w <= wordValue w2)
                         
minWord :: Word
minWord = Word (letter '_', letter '_', letter '_', letter '_')

maxWord = Word (letter 'Z', letter 'Z', letter 'Z', letter 'Z')
          
-- | The number of possible values that a word can have.
wordValues :: Int
wordValues = 27 ^ 4

-- | Turns a word into a list of letters.
wordToList :: Word -> [Letter]
wordToList (Word (a,b,c,d)) = [a,b,c,d]

-- | Turns a list of letters into a word. Beware, this is a partial function.
wordFromList :: [Letter] -> Word
wordFromList (a:b:c:d:_) = Word (a,b,c,d)
wordFromList (a:b:c:_) = Word (letter '_', a, b, c)
wordFromList (a:b:_) = Word (letter '_', letter '_', a, b)
wordFromList (a:_) = Word (letter '_', letter '_', letter '_', a)
wordFromList (_) = minWord

wordValue :: Word -> Int
--wordValue word = read $ concat $ map (show) $ convertBase 27 10 (map getValue (wordToList word))
wordValue = unDigits 27 . map getValue . wordToList

toWord :: Int -> Word
toWord =  wordFromList . map toLetter . digits 27 . (`mod` wordValues)

-- | Extends a letter to a word with the same numeric value.
extendToWord :: Letter -> Word
extendToWord letter_ = Word (letter '_', letter '_', letter '_', letter_)

lastLetter :: Word -> Letter
lastLetter (Word (a,b,c,d)) = d

-- | Adds two words.
addWord :: Word -> Word -> Word
addWord w1 w2 = toWord $ (wordValue w1) + (wordValue w2)

-- | Finds the word that occurs diff times after the initial word.
offset :: Word -> Int -> Word
offset w diff = addWord w $ toWord diff
                
-- | Debug / Convenience function to make a word from a string.
wrd :: String -> Word
wrd (a:b:c:d:[]) = Word (letter a, letter b, letter c, letter d)
