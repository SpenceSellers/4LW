{-# LANGUAGE LambdaCase #-}
module Io (readToBuffer, charToInternal, internalToChar) where

import Base27
import System.IO
import Data.Monoid
import Data.Char
import Data.Ix
import Control.Lens
import Debug.Trace

__ = letter '_'

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
    | inRange ('a', 'z') c = Base27.Word __ __ (letter 'A') (letter . toUpper $ c)
    | inRange ('0', '9') c = Base27.Word __ __ (letter 'N') (Base27.toLetter . read $ [c])
    | c == '\n' = Base27.Word __ __ (letter 'C') (letter '_')
    | c == ':' = Base27.Word __ __ (letter 'P') (letter 'C')
    | c == '%' = Base27.Word __ __ (letter 'P') (letter 'P')
    | c == '!' = Base27.Word __ __ (letter 'P') (letter 'X')
    | c == '.' = Base27.Word __ __ (letter 'P') (letter 'D')
    | c == '-' = Base27.Word __ __ (letter 'P') (letter 'M')
    | c == '|' = Base27.Word __ __ (letter 'P') (letter 'B')
    | c == '(' = Base27.Word __ __ (letter 'B') (letter 'A')
    | c == ')' = Base27.Word __ __ (letter 'B') (letter 'B')


internalToChar :: Base27.Word -> Maybe Char
internalToChar (WordChars '_' '_' '_' c) = Just $ toUpper c
internalToChar (WordChars '_' '_' 'A' '_') = Just ' '
internalToChar (WordChars '_' '_' 'A' c) = Just $ toLower c
internalToChar (WordChars '_' '_' 'N' c) = toDigit (letter c)
internalToChar (WordChars '_' '_' 'C' '_') = Just '\n'
internalToChar (WordChars '_' '_' 'P' 'C') = Just ':'
internalToChar (WordChars '_' '_' 'P' 'P') = Just '%'
internalToChar (WordChars '_' '_' 'P' 'X') = Just '!'
internalToChar (WordChars '_' '_' 'P' 'D') = Just '.'
internalToChar (WordChars '_' '_' 'P' 'M') = Just '-'
internalToChar (WordChars '_' '_' 'P' 'B') = Just '|'
internalToChar (WordChars '_' '_' 'B' 'A') = Just '('
internalToChar (WordChars '_' '_' 'B' 'B') = Just ')'

internalToChar (WordChars a b c d) = trace (a:b:c:d:" Invalid") Nothing

toDigit :: Letter -> Maybe Char
toDigit l = range ('0', '9') ^? ix (getValue l)
