-- |Handles the main memory of 4LW (Think RAM/Heap)
-- The main memory consists of 27^4 letters of memory.
-- Letters are individually addressable, so to get to the next 
-- word you have to increment a pointer by four.

{-# LANGUAGE BangPatterns #-}
module Memory where
import Prelude hiding (Word)
import Base27
import Lengths
import qualified Data.Map as Map
import Control.Applicative
import Data.Ix
import Data.Default
import Control.Lens
import Control.Monad.Identity

type Memory = Map.Map Word Letter

data MemoryError = AddressOverrun deriving (Show, Eq)

-- |A blank memory
blankMemory :: Memory
blankMemory = Map.empty

-- |Read a letter from memory
readLetter :: Memory -> Word -> Letter
readLetter mem addr = Map.findWithDefault (letter '_') addr mem

-- |Read a sequence of letters from memory
readLetters :: Memory -> Word -> LetterLength -> [Letter]
readLetters mem addr (LetterLength len) = map (readLetter mem) addrs
    where addrs = map (offset addr) [0 .. len - 1]

-- |Read an entire word from memory
readWord :: Memory -> Word -> Word
readWord mem addr = Word a b c d
    where (a,b,c,d) = wordAddrMap (readLetter mem) addr

-- |Writes a single letter to memory, given an address.
writeLetter :: Memory -> Word -> Letter -> Memory
writeLetter mem addr (LetterV '_') = Map.delete addr mem -- Deleting zero letters yields a small speed increase.
writeLetter mem addr letter = Map.insert addr letter mem

-- |Writes a sequence of letters to memory, given a starting address.
writeLetters :: Memory -> Word -> [Letter] -> Memory
writeLetters mem addr (l:rest) =
    writeLetters (writeLetter mem addr l) (offsetBy addr (LetterLength 1)) rest
writeLetters mem _ [] = mem

-- |Todo: Check for end of bounds
writeWord :: Memory -> Word -> Word -> Memory
writeWord mem addr (Word a b c d) =
    foldl (\m (addr', letter') -> Map.insert addr' letter' m) mem
        [(addr0, a), (addr1, b), (addr2, c), (addr3, d)]
    where (addr0, addr1, addr2, addr3) = wordAddrs addr

-- |Reads an entire range of words.
readWords :: Memory -> Word -> WordLength -> [Word]
readWords mem addr (WordLength len) = map (readWord mem) addrs
    where addrs = map (offset addr) [0,4..(len - 1)*4]

-- |Takes a range of memory and outputs it as a string for 
-- the user's viewing pleasure.
exportString :: Memory -> (Word, Word) -> String
exportString mem (start, end) =
    map (\addr -> getletter $ readLetter mem addr) $ range (start, end)
        where getletter (LetterV c) = c

-- |Takes a string and inserts it into memory.
importString :: String -> Word -> Memory -> Maybe Memory
importString str addr mem = writeLetters mem addr <$> sequence (map letterSafe str)

-- |Like importString, but it creates a new Memory.
makeMem :: String -> Maybe Memory
makeMem s = importString s minWord blankMemory

orBlank :: Default a => Either MemoryError a -> a
orBlank = either (const def) id

-- |Calculates the letter addresses of a word given a starting word.
-- At the moment this does wrap around.
wordAddrs :: Word -> (Word, Word, Word, Word)
wordAddrs start = (start, offset start 1, offset start 2, offset start 3)

-- |Calculates the addresses of the letters of a word given a starting addr,
-- and gives the addresses to a monadic function.
wordAddrApply :: Monad m => (Word -> m a) -> Word -> m (a,a,a,a)
wordAddrApply f addr = sequenceOf each (f a0, f a1, f a2, f a3)
    where (a0, a1, a2, a3) = wordAddrs addr

-- |Maps a function to each letter in a word.
wordAddrMap :: (Word -> a) -> Word -> (a, a, a, a)
wordAddrMap f addr = (f a0, f a1, f a2, f a3)
    where (a0, a1, a2, a3) = wordAddrs addr
