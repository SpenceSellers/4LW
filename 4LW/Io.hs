{-# LANGUAGE LambdaCase #-}
module Io (readToBuffer, charToInternal, internalToChar, printChar, prepareTerminal, unprepareTerminal) where

import Base27
import System.IO
import Data.Monoid
import Data.Char
import Data.Ix
import Control.Lens
import Data.Maybe
import Debug.Trace
import System.IO

import System.Console.ANSI

__ = letter '_'

uppercaseTable = map (\c -> (c, Base27.Word __ __ __ (letter c))) $ range ('A', 'Z')
lowercaseTable = map (\c -> (c, Base27.Word __ __ (letter 'A') (letter . toUpper $ c))) $ range ('a', 'z')
numberTable = map (\c -> (c, Base27.Word __ __ (letter 'N') (Base27.toLetter . read $ [c]))) $ range ('0', '9')
charTable = uppercaseTable ++ lowercaseTable ++ numberTable ++ [
    ('_', wrd "____"),
    (' ', wrd "__A_"),
    ('\n', wrd "__C_"),
    -- ('\b', wrd "__CB"),
    ('\DEL', wrd "__CB"),
    (':', wrd "__PC"),
    ('%', wrd "__PP"),
    ('!', wrd "__PX"),
    ('.', wrd "__PD"),
    ('-', wrd "__PM"),
    ('?', wrd "__PQ"),
    ('|', wrd "__PB"),
    ('(', wrd "__BA"),
    (')', wrd "__BB")
    ]

tr x = trace (show x) x

readToBuffer :: [Char] -> IO [Char]
readToBuffer buf = do
    isReady <- hReady stdin
    if isReady
        then do
            c <- hGetChar stdin
            --trace (show c) (return ())
            readToBuffer (buf ++ [c])
        else
            return buf

charToInternal :: Char -> Maybe Base27.Word
charToInternal c = lookup c charTable

internalToChar :: Base27.Word -> Maybe Char
internalToChar word = lookup word . map swap $ charTable
    where swap (a,b) = (b,a)

printChar :: Base27.Word -> IO ()
printChar w
    -- On backspace, move back a char, write a space back over it, and move backwards again.
    | w == wrd "__CB" = cursorBackward 1 >> putStr " " >> cursorBackward 1 >> hFlush stdout
    | otherwise = case internalToChar w of
        Just c -> do
            putStr [c]
            hFlush stdout
        Nothing -> return ()


toDigit :: Letter -> Maybe Char
toDigit l = range ('0', '9') ^? ix (getValue l)

prepareTerminal :: IO ()
prepareTerminal = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

unprepareTerminal :: IO ()
unprepareTerminal = do
    hSetBuffering stdin LineBuffering
    hSetEcho stdin True
