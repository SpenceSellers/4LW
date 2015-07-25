module Lengths where
import Base27

newtype LetterLength = LetterLength Int
newtype WordLength = WordLength Int

class ToLetterLength a where
    letterLen :: a -> Int
    letterLen x = n where LetterLength n = toLetterLength x

    toLetterLength :: a -> LetterLength

instance ToLetterLength LetterLength where
    toLetterLength l = l

instance ToLetterLength WordLength where
    toLetterLength (WordLength n) = LetterLength $ n * 4

addLengths :: (ToLetterLength l1, ToLetterLength l2) =>  l1 -> l2 -> LetterLength
addLengths a b = LetterLength $ (letterLen a) + (letterLen b)

addWordLengths :: WordLength -> WordLength -> WordLength
addWordLengths (WordLength a) (WordLength b) = WordLength $ a + b
