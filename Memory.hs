module Memory where
import Base27
import Data.Array
type Memory = Array Word Letter

data MemoryError = AddressOverrun deriving (Show, Eq)
                 
blankMemory :: Memory
blankMemory = listArray (minWord, maxWord) (repeat (Letter '_'))

readLetter :: Memory -> Word -> Letter
readLetter mem addr = mem ! addr

-- |TODO: Check for end-of-bounds.
readWord :: Memory -> Word -> Either MemoryError Word
readWord mem addr = Right $ Word (readLetter mem addr,
                          readLetter mem (offset addr 1),
                          readLetter mem (offset addr 2),
                          readLetter mem (offset addr 3))

writeLetter :: Memory -> Word -> Letter -> Memory
writeLetter mem addr letter = mem // [(addr, letter)]

-- |Todo: Check for end of bounds
writeWord :: Memory -> Word -> Word -> Memory
writeWord mem addr (Word (a,b,c,d)) =
    mem // [(addr0, a),
            (addr1, b),
            (addr2, c),
            (addr3, d)]
    where addr0 = addr
          addr1 = offset addr 1
          addr2 = offset addr 2
          addr3 = offset addr 3

readWords :: Memory -> Word -> Int -> Either MemoryError [Word]
readWords mem addr len = mapM (readWord mem) addrs
    where addrs = map (offset addr) [1..len]
