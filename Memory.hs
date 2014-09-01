module Memory where
import Base27
import Data.Array
type Memory = Array Word Letter

blankMemory :: Memory
blankMemory = listArray (minWord, maxWord) (repeat (Letter '_'))

readLetter :: Memory -> Word -> Letter
readLetter mem addr = mem ! addr

-- |TODO: Check for end-of-bounds.
readWord :: Memory -> Word -> Word
readWord mem addr = Word (readLetter mem addr,
                          readLetter mem (addWord addr $ toWord 1),
                          readLetter mem (addWord addr $ toWord 2),
                          readLetter mem (addWord addr $ toWord 3))

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
          addr1 = addWord addr $ toWord 1
          addr2 = addWord addr $ toWord 2
          addr3 = addWord addr $ toWord 3
