{-# LANGUAGE PatternSynonyms #-}

module Base27 (Letter, Word(Word) , letter,
               letterSafe, getValue,
               toLetter, offset, wrd, toWord,
               extendToWord, pattern LetterV, pattern WordChars,
               minWord, maxWord, letter2, valueOfWord, wordValue, wordFromList, wordFromList',
               negateWord, addWord, subWord, mulWord, modWord, andWord,
               leftShift, rightShift,
               divWord, isLetter, letters,
               firstLetter, secondLetter, thirdLetter, fourthLetter) where
import Prelude hiding (Word)
import Data.Char hiding (isLetter)
import Data.Ix
import Debug.Trace
import Data.Digits (digits, unDigits)
import Data.Default
import Control.Lens

newtype Letter = Letter Char deriving (Eq)

-- | A view for letter. You can pattern match on it, but you can't construct it!
pattern LetterV c <- Letter c
pattern WordChars a b c d <- Word (Letter a) (Letter b) (Letter c) (Letter d)

instance Ord Letter where
    -- Letter being used instead of LetterV for performance.
    compare (Letter '_') (Letter '_') = EQ
    compare (Letter '_') (Letter _) = LT
    compare (Letter _) (Letter '_') = GT
    compare (Letter a) (Letter b) = compare a b

instance Ix Letter where
    range (l1, l2) = map toLetter $ range (n1, n2)
        where
          n1 = getValue l1
          n2 = getValue l2

    index (l1, l2) l = (getValue l) - (getValue l1)

    inRange (l1, l2) l = (getValue l >= getValue l1) && (getValue l <= getValue l2)

instance Show Letter where
    show (LetterV c) = "." ++ [c]

instance Default Letter where
    def = letter '_'

-- | Better constructor, with checking.
letter :: Char -> Letter
letter c = if Base27.isLetter c then Letter c else error "Bad letter value!"

letterSafe :: Char -> Maybe Letter
letterSafe c
    | Base27.isLetter c = Just (letter c)
    | otherwise = Nothing

isLetter :: Char -> Bool
isLetter '_' = True
isLetter c = inRange ('A', 'Z') c

-- | Gets the numeric value of a Letter.
getValue :: Letter -> Int
getValue (LetterV '_') = 0
getValue (LetterV l) = Data.Ix.index ('A', 'Z') l + 1

-- | Converts a integer (0,26) to a letter. Yes this is a partial function.
toLetter :: Int -> Letter
toLetter num
    | num == 0 = letter '_'
    | num < 0 = error ("toLetter must be positive: " ++ show num)
    | num > 26 = error "toLetter must be 26 or below."
    | otherwise = letter $ chr (ord 'A' + num - 1)

toLetterSafe :: Int -> Maybe Letter
toLetterSafe num
    | num < 0 = Nothing
    | num > 26 = Nothing
    | otherwise = Just $ toLetter num

convertBase :: Integral a => a -> a -> [a] -> [a]
convertBase from to = digits to . unDigits from

andLetter :: Letter -> Letter -> Letter
andLetter a b = toLetter (((getValue a) * (getValue b)) `mod` 27)

---------------------
-------- WORDS ------
---------------------

-- | A word is basically a tuple of four letters.
data Word = Word Letter Letter Letter Letter
            deriving (Eq, Ord)

-- | The default Show is hard to read, let's just cram the letters together.
instance Show Base27.Word where
    show word =
        [ca, cb, cc, cd] where
            (Word a b c d) = word
            (LetterV ca) = a
            (LetterV cb) = b
            (LetterV cc) = c
            (LetterV cd) = d

instance Ix Base27.Word where
    range (w1, w2) = map toWord $ range (n1, n2)
        where
          n1 = wordValue w1
          n2 = wordValue w2

    index (w1, w2) w = (wordValue w) - (wordValue w1)

    inRange (w1, w2) w = (wordValue w >= wordValue w1) && (wordValue w <= wordValue w2)

instance Enum Base27.Word where
    toEnum = toWord
    fromEnum = wordValue

firstLetter :: Lens' Word Letter
firstLetter f (Word a b c d) = (\new -> Word new b c d) <$> f a

secondLetter :: Lens' Word Letter
secondLetter f (Word a b c d) = (\new -> Word a new c d) <$> f b

thirdLetter :: Lens' Word Letter
thirdLetter f (Word a b c d) = (\new -> Word a b new d) <$> f c

fourthLetter :: Lens' Word Letter
fourthLetter f (Word a b c d) = (\new -> Word a b c new) <$> f d

valueOfWord :: Iso' Word Int
valueOfWord = iso wordValue toWord

valueOfLetter :: Prism' Int Letter
valueOfLetter = prism' getValue toLetterSafe

minWord :: Base27.Word
minWord = Word (letter '_') (letter '_') (letter '_') (letter '_')

maxWord :: Base27.Word
maxWord = Word (letter 'Z') (letter 'Z') (letter 'Z') (letter 'Z')

-- | The number of possible values that a word can have.
wordValues :: Int
wordValues = 27 ^ 4

-- | Turns a word into a list of letters.
wordToList :: Base27.Word -> [Letter]
wordToList (Word a b c d) = [a,b,c,d]

-- | Turns a list of letters into a word. Beware, this is a partial function.
wordFromList :: [Letter] -> Base27.Word
wordFromList (a:b:c:d:_) = Word a b c d
wordFromList (a:b:c:_) = Word (letter '_') a b c
wordFromList (a:b:_) = Word (letter '_') (letter '_') a b
wordFromList (a:_) = Word (letter '_') (letter '_') (letter '_') a
wordFromList (_) = minWord

wordFromList' :: [Letter] -> Base27.Word
wordFromList' (a:b:c:d:_) = Word a b c d
wordFromList' (a:b:c:_) = Word a b c (letter '_')
wordFromList' (a:b:_) = Word a b (letter '_') (letter '_')
wordFromList' (a:_) = Word a (letter '_') (letter '_') (letter '_')
wordFromList' (_) = minWord

wordValue :: Base27.Word -> Int
--wordValue = unDigits 27 . map getValue . wordToList
-- Optimized expanded version:
wordValue (Word a b c d) = (19683 * (getValue a)) + (729 * (getValue b)) + (27 * (getValue c)) + (getValue d)

toWord :: Int -> Base27.Word
--toWord = wordFromList . map toLetter . digits 27 . (`mod` wordValues)
toWord num = Word (toLetter a) (toLetter b) (toLetter c) (toLetter d)
    where (a,b,c,d) = toWordDigits num

toWordDigits :: Int -> (Int, Int, Int, Int)
--{-# INLINE toWordDigits #-}
toWordDigits val = (a, b, c, d)
    where n = val `mod` wordValues
          (dr, d) = quotRem n 27
          (cr, c) = quotRem dr 27
          (br, b) = quotRem cr 27
          (_, a) = quotRem br 27

letters :: Traversal' Word Letter
letters f (Word a b c d) = Word <$> (f a) <*> (f b) <*> (f c) <*> (f d)

-- | Extends a letter to a word with the same numeric value.
extendToWord :: Letter -> Base27.Word
extendToWord l = Word (letter '_') (letter '_') (letter '_') l

-- | Adds two words.
addWord :: Base27.Word -> Base27.Word -> Base27.Word
addWord w1 w2 = toWord $ (wordValue w1) + (wordValue w2)

subWord :: Base27.Word -> Base27.Word -> Base27.Word
subWord w1 w2 = toWord $ (wordValue w1) - (wordValue w2)

mulWord :: Base27.Word -> Base27.Word -> Base27.Word
mulWord w1 w2 = toWord $ (wordValue w1) * (wordValue w2)

divWord :: Base27.Word -> Base27.Word -> Base27.Word
divWord w1 w2 = toWord $ (wordValue w1) `div` (wordValue w2)

modWord :: Base27.Word -> Base27.Word -> Base27.Word
modWord w1 w2 = toWord $ (wordValue w1) `mod` (wordValue w2)

negateWord :: Base27.Word -> Base27.Word
negateWord = subWord (wrd "ZZZZ")

rightShift :: Word -> Word
rightShift (Word a b c d) = Word (letter ' ') a b c

leftShift :: Word -> Word
leftShift (Word a b c d) = Word b c d (letter ' ')

andWord :: Word -> Word -> Word
andWord a b = zipWord andLetter a b

offset :: Word -> Int -> Word
offset w diff = valueOfWord +~ diff $ w

-- | Debug / Convenience function to make a word from a string.
wrd :: String -> Base27.Word
wrd (a:b:c:d:[]) = Word (letter a) (letter b) (letter c) (letter d)

wrdSafe :: String -> Maybe Base27.Word
wrdSafe (a:b:c:d:[]) = Word <$> (letterSafe a) <*> (letterSafe b) <*> (letterSafe c) <*> (letterSafe d)
wrdSafe _ = Nothing

wordToString :: Base27.Word -> String
wordToString word = map getLetter (wordToList word)
    where getLetter (LetterV c) = c

lettersFromString :: String -> Maybe [Letter]
lettersFromString = sequence . fmap letterSafe


letter2 :: String -> (Letter, Letter)
letter2 (a:b:[]) = (letter a, letter b)

zipWord :: (Letter -> Letter -> Letter ) -> Word -> Word -> Word
zipWord f (Word a b c d) (Word w x y z) =
    Word (f a w) (f b x) (f c y) (f d z)
