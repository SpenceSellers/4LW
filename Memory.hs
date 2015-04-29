module Memory where
import Prelude hiding (Word)
import Base27
import Data.Array
import Control.Applicative
type Memory = Array Word Letter

data MemoryError = AddressOverrun deriving (Show, Eq)

data MemoryWrite = MemoryWrite Word Letter deriving (Show, Eq)
                 
blankMemory :: Memory
blankMemory = listArray (minWord, maxWord) (repeat (Letter '_'))

readLetter :: Memory -> Word -> Either MemoryError Letter
readLetter mem addr = if inRange (bounds mem) addr then
                          Right $ mem ! addr
                      else
                          Left AddressOverrun

-- |TODO: Check for end-of-bounds.
readWord :: Memory -> Word -> Either MemoryError Word
readWord mem addr = do
  a <- readLetter mem addr
  b <- readLetter mem (offset addr 1)
  c <- readLetter mem (offset addr 2)
  d <- readLetter mem (offset addr 3)
  return $ Word a b c d

writeLetter :: Memory -> Word -> Letter -> Memory
writeLetter mem addr letter = mem // [(addr, letter)]

writeLetters :: Memory -> Word -> [Letter] -> Memory
writeLetters mem addr (l:rest) =
    writeLetters (writeLetter mem addr l) (offset addr 1) rest
writeLetters mem _ [] = mem

applyWrite :: Memory -> MemoryWrite -> Memory
applyWrite mem (MemoryWrite addr letter) =
    writeLetter mem addr letter

-- |Todo: Check for end of bounds
writeWord :: Memory -> Word -> Word -> Memory
writeWord mem addr (Word a b c d) =
    mem // [(addr0, a),
            (addr1, b),
            (addr2, c),
            (addr3, d)]
    where addr0 = addr
          addr1 = offset addr 1
          addr2 = offset addr 2
          addr3 = offset addr 3
                      
-- |Reads an entire range of words.
-- |The "length" is still in number of letters!
readWords :: Memory -> Word -> Int -> Either MemoryError [Word]
readWords mem addr len = mapM (readWord mem) addrs
    where addrs = map (offset addr) [0,4..len*4]

exportString :: Memory -> (Word, Word) -> String
exportString mem (start, end) =
    map (\x -> (getletter $ mem ! x)) $ range (start, end)
        where getletter (Letter c) = c

importString :: String -> Word -> Memory -> Maybe Memory
importString str addr mem = writeLetters mem addr <$> sequence (map letterSafe str)
