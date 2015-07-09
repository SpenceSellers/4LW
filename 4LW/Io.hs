{-# LANGUAGE LambdaCase #-}
module Io where

import Base27
import System.IO
import Data.Monoid

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
charToInternal c = minWord

internalToChar :: Base27.Word -> Char
internalToChar w = c
    where (Letter c) = Base27.lastLetter w
