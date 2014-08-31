module Base27 where

import Data.Char
import Data.Ix

import Data.Digits (digits, unDigits)

newtype Letter = Letter Char deriving (Show, Ord, Eq)

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
