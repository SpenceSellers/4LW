module Memory where
import Prelude hiding (Word)
import Base27
import Data.Array
import qualified Data.Map as Map
import Control.Applicative
import Data.Default
import Control.Lens
import Control.Monad.Identity
type Memory = Map.Map Word Letter

data MemoryError = AddressOverrun deriving (Show, Eq)

blankMemory :: Memory
blankMemory = Map.empty

readLetter :: Memory -> Word -> Letter
readLetter mem addr =  Map.findWithDefault (letter '_') addr mem


readLetters :: Memory -> Word -> Int -> [Letter]
readLetters mem addr len = map (readLetter mem) addrs
    where addrs = map (offset addr) [0 .. len - 1]

readWord :: Memory -> Word -> Word
readWord mem addr = Word a b c d
    where (a,b,c,d) = wordAddrMap (readLetter mem) addr

-- |Writes a single letter to memory, given an address.
writeLetter :: Memory -> Word -> Letter -> Memory
writeLetter mem addr letter = Map.insert addr letter mem

-- |Writes a sequence of letters to memory, given a starting address.
writeLetters :: Memory -> Word -> [Letter] -> Memory
writeLetters mem addr (l:rest) =
    writeLetters (writeLetter mem addr l) (offset addr 1) rest
writeLetters mem _ [] = mem

-- |Todo: Check for end of bounds
writeWord :: Memory -> Word -> Word -> Memory
writeWord mem addr (Word a b c d) =
    foldl (\m (addr', letter') -> Map.insert addr' letter' m) mem
        [(addr0, a), (addr1, b), (addr2, c), (addr3, d)]
    where (addr0, addr1, addr2, addr3) = wordAddrs addr

-- |Reads an entire range of words.
-- |The "length" is still in number of letters!
readWords :: Memory -> Word -> Int -> [Word]
readWords mem addr len = map (readWord mem) addrs
    where addrs = map (offset addr) [0,4..len*4]

exportString :: Memory -> (Word, Word) -> String
exportString mem (start, end) =
    map (\addr -> getletter $ readLetter mem addr) $ range (start, end)
        where getletter (LetterV c) = c


importString :: String -> Word -> Memory -> Maybe Memory
importString str addr mem = writeLetters mem addr <$> sequence (map letterSafe str)

orBlank :: Default a => Either MemoryError a -> a
orBlank = either (const def) id

-- |Calculates the addresses of a word given a starting word.
-- At the moment this does wrap around.
wordAddrs :: Word -> (Word, Word, Word, Word)
wordAddrs start = (start, offset start 1, offset start 2, offset start 3)

-- |Calculates the addresses of the letters of a word given a starting addr,
-- and gives the addresses to a monadic function.
wordAddrApply :: Monad m => (Word -> m a) -> Word -> m (a,a,a,a)
wordAddrApply f addr = sequenceOf each (f a0, f a1, f a2, f a3)
    where (a0, a1, a2, a3) = wordAddrs addr

wordAddrMap :: (Word -> a) -> Word -> (a, a, a, a)
wordAddrMap f addr = (f a0, f a1, f a2, f a3)
    where (a0, a1, a2, a3) = wordAddrs addr
