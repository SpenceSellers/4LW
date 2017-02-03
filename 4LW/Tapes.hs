-- | The 4LW machine can have up to 27 tape drives attached to it.
-- Tapes hold quite a bit of storage (27^4 words), but are not directly 
-- addressable. Only seek / rewind operations are provided in hardware.

module Tapes where
import Prelude hiding (Word)
import Base27
import WordSequence

import qualified Data.Map as Map
import Control.Monad.State.Lazy
import Control.Lens
import Control.Applicative

-- | Contains all of the tapes that a 4LW virtual machine has access to.
type TapeDeck = Map.Map Letter Tape

blankTapeDeck = Map.empty

data Tape = Tape Word (Map.Map Word Word)
    deriving (Show)

-- | Lens onto the position of the tape's imaginary read/write head.
tapePos :: Lens' Tape Word
tapePos f (Tape pos contents) = (\new -> Tape new contents) <$> f pos

-- | Lens onto the map that makes up the tape's real content.
tapeContents :: Lens' Tape (Map.Map Word Word)
tapeContents f (Tape pos contents) = (\new -> Tape pos new) <$> f contents

blankTape = Tape minWord Map.empty

newTape :: [Word] -> Tape
newTape words = execState (tapeWriteWords words >> tapeRewind) blankTape

-- | Reads a word from the tape map.
readWord :: Map.Map Word Word -> Word -> Word
readWord tapemap addr = Map.findWithDefault minWord addr tapemap

-- | Read a tape, advancing the position.
tapeRead :: Monad m => StateT Tape m Word
tapeRead = do
    Tape pos contents <- get
    tapeForward
    return (readWord contents pos)

-- | Write to a tape, advancing the position.
tapeWrite :: Monad m => Word -> StateT Tape m ()
tapeWrite word = do
    Tape pos contents <- get
    tapeContents .= Map.insert pos word contents
    tapeForward

-- | Write multiple words to a tape, advancing the position.
tapeWriteWords :: Monad m => [Word] -> StateT Tape m ()
tapeWriteWords [] = return ()
tapeWriteWords (x:xs) = do
    tapeWrite x
    tapeWriteWords xs

-- | Advance the tape by one word.
tapeForward :: Monad m => StateT Tape m ()
tapeForward = tapeSeekForward (toWord 1)

-- | Seeks the R/W head forwards without reading or writing.
tapeSeekForward :: Monad m => Word -> StateT Tape m ()
tapeSeekForward dist = tapePos %= addWord dist

-- | Seeks the R/W head backwards without reading or writing.
tapeSeekBackwards :: Monad m => Word -> StateT Tape m ()
tapeSeekBackwards dist = tapePos %= flip subWord dist

-- | Rewind a tape to the very beginning.
tapeRewind :: Monad m => StateT Tape m ()
tapeRewind = tapePos .= minWord

-- | Reads a file and turns it into a tape.
readTapeFromFile :: String -> IO (Maybe Tape)
readTapeFromFile filename = do
    maybeWs <- readFileWords filename
    case maybeWs of
        Just ws -> return $ Just (newTape ws)
        Nothing -> return Nothing

-- | Convert a tape to a list of words.
tapeToList :: Tape -> [Word]
tapeToList (Tape _ tapemap) = map (readWord tapemap) [minWord .. maxkey]
    where (maxkey, _) = Map.findMax tapemap

writeTapeToFile :: String -> Tape -> IO ()
writeTapeToFile filename tape = writeFile filename . wordsToString . tapeToList $ tape
