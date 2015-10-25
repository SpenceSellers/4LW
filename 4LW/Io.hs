{-# LANGUAGE LambdaCase #-}
module Io (readToBuffer, charToInternal, internalToChar) where

import Base27
import System.IO
import Data.Monoid
import Data.Char
import Data.Ix
import Control.Lens
import Data.Maybe
import Debug.Trace

__ = letter '_'

uppercaseTable = map (\c -> (c, Base27.Word __ __ __ (letter c))) $ range ('A', 'Z')
lowercaseTable = map (\c -> (c, Base27.Word __ __ (letter 'A') (letter . toUpper $ c))) $ range ('a', 'z')
numberTable = map (\c -> (c, Base27.Word __ __ (letter 'N') (Base27.toLetter . read $ [c]))) $ range ('0', '9')
charTable = uppercaseTable ++ lowercaseTable ++ numberTable ++ [
    ('_', wrd "____"),
    (' ', wrd "__A_"),
    ('\n', wrd "__C_"),
    (':', wrd "__PC"),
    ('%', wrd "__PP"),
    ('!', wrd "__PX"),
    ('.', wrd "__PD"),
    ('-', wrd "__PM"),
    ('|', wrd "__PB"),
    ('(', wrd "__BA"),
    (')', wrd "__BB")]


readToBuffer :: [Char] -> IO [Char]
readToBuffer buf = do
    isReady <- hReady stdin
    if isReady
        then do
            c <- hGetChar stdin
            readToBuffer (buf ++ [c])
        else
            return buf

charToInternal :: Char -> Maybe Base27.Word
charToInternal c = lookup c charTable

internalToChar :: Base27.Word -> Maybe Char
internalToChar word = lookup word . map swap $ charTable
    where swap (a,b) = (b,a)

toDigit :: Letter -> Maybe Char
toDigit l = range ('0', '9') ^? ix (getValue l)
