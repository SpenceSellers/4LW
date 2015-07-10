{-# LANGUAGE LambdaCase #-}
module Io where

import Base27
import System.IO
import Data.Monoid
import Data.Char
import Data.Ix

readToBuffer :: [Char] -> IO [Char]
readToBuffer buf = do
    isReady <- hReady stdin
    if isReady
        then do
            c <- hGetChar stdin
            readToBuffer (buf ++ [c])
        else
            return buf

charToInternal :: Char -> Base27.Word
charToInternal c
    | c == ' ' = wrd "__A_"
    | c == '_' = wrd "____"
    | inRange ('A', 'Z') c = extendToWord $ letter c
    | inRange ('a', 'z') c = Base27.Word (letter '_') (letter '_') (letter 'A') (letter . toUpper $ c)

internalToChar :: Base27.Word -> Char
internalToChar (Word a b c d) = char
    where Letter char = d
