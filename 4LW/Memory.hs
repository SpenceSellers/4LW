module Memory where
import Prelude hiding (Word)
import Base27
import Data.Array
import qualified Data.Map as Map
import Control.Applicative
import Data.Default
type Memory = Map.Map Word Letter

data MemoryError = AddressOverrun deriving (Show, Eq)

blankMemory :: Memory
blankMemory = Map.empty

readLetter :: Memory -> Word -> Either MemoryError Letter
readLetter mem addr = Right $ Map.findWithDefault (letter '_') addr mem


readLetters :: Memory -> Word -> Int -> Either MemoryError [Letter]
readLetters mem addr len = mapM (readLetter mem) addrs
    where addrs = map (offset addr) [0 .. len - 1]

readWord :: Memory -> Word -> Either MemoryError Word
readWord mem addr = do
  a <- readLetter mem addr
  b <- readLetter mem (offset addr 1)
  c <- readLetter mem (offset addr 2)
  d <- readLetter mem (offset addr 3)
  --[a,b,c,d] <- readLetters mem addr 4 -- This alternative proved to be slightly slower.
  return $ Word a b c d

writeLetter :: Memory -> Word -> Letter -> Memory
writeLetter mem addr letter = Map.insert addr letter mem

writeLetters :: Memory -> Word -> [Letter] -> Memory
writeLetters mem addr (l:rest) =
    writeLetters (writeLetter mem addr l) (offset addr 1) rest
writeLetters mem _ [] = mem

-- |Todo: Check for end of bounds
writeWord :: Memory -> Word -> Word -> Memory
writeWord mem addr (Word a b c d) =
    foldl (\m (addr', letter') -> Map.insert addr' letter' m) mem
        [(addr0, a), (addr1, b), (addr2, c), (addr3, d)]
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
    map (\addr -> getletter $ orBlank $ readLetter mem addr) $ range (start, end)
        where getletter (LetterV c) = c


importString :: String -> Word -> Memory -> Maybe Memory
importString str addr mem = writeLetters mem addr <$> sequence (map letterSafe str)

orBlank :: Default a => Either MemoryError a -> a
orBlank = either (const def) (id)
