
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Machine where
import Prelude hiding (Word)

import Data.Array
import Data.Ix
import Instruction
import Base27
import Lengths
import Registers
import qualified Memory
import qualified Tapes
import qualified Io
import qualified Stacks
import Control.Lens
import Control.Monad
import Data.Maybe
import Control.Monad.State.Lazy
import Control.Applicative
import Control.Monad.Reader
import Debug.Trace
import Control.Concurrent
import System.IO

returnAddressStackId :: Letter
returnAddressStackId = letter 'R'

returnValueStackId :: Letter
returnValueStackId = letter 'V'

argStackId :: Letter
argStackId = letter 'S'

preserveStackId :: Letter
preserveStackId = letter 'P'

-- | Brings a function into the State monad.
hoistState :: Monad m => State s a -> StateT s m a
hoistState = StateT . (return .) . runState

data IOConfig = IOConfig {
    ioPutChar :: Char -> IO (),
    ioGetChar :: IO (Maybe Char)
}

data MachineAction = NoAction |
                     HaltAction |
                     IOWrite [Word]
                     deriving (Show, Eq)

-- | Stores the entire machine state from one instruction to the next.
data MachineState = MachineState {
      _registers :: Registers,
      _memory :: Memory.Memory,
      _stacks :: Stacks.Stacks,
      _tapeDeck :: Tapes.TapeDeck,
      _action :: MachineAction,
      _tickNum :: Integer,
      _inBuffer :: [Char],
      _outBuffer :: [Word]
    } deriving (Show)

makeLenses ''MachineState

-- | A blank "starting" state of the machine, with everything zeroed.
blankState :: MachineState
blankState = MachineState blankRegisters Memory.blankMemory Stacks.emptyStacks Tapes.blankTapeDeck NoAction 0 [] []

-- | Pops a char off of the machine's input buffer.
popInBuffer :: State MachineState (Maybe Char)
popInBuffer = do
  buf <- use inBuffer
  case buf of
    x:xs -> do
        inBuffer .= xs
        return (Just x)
    [] -> return Nothing

setRegister :: Letter -> Word -> State MachineState ()
-- At the moment this will do nothing if the letter is not a valid register.
setRegister r w = registers %= (\regs -> fromMaybe regs $ updateRegister regs r w)

setMemory :: Word -> Word -> State MachineState ()
setMemory addr word = memory %= \mem -> Memory.writeWord mem addr word

pushStack :: Letter -> Word -> State MachineState ()
pushStack l word = do
    stks <- use stacks
    let stack = stks ! l
    -- Currently does nothing if the stack is full. This will definitely need to change.
    let newStack = fromMaybe (trace "Stack overflow!" stack) $ Stacks.push stack word
    let newStacks = stks // [(l, newStack)]
    stacks .= newStacks

popStack :: Letter -> State MachineState Word
popStack l = do
    stks <- use stacks
    pc <- use registers
    let stack = stks ! l
    let (word, newStack) = fromMaybe (trace ("Stack underflow! on " ++ show l ++ " at " ++ show pc) (minWord, stack)) $ Stacks.pop stack
    let newStacks = stks // [(l, newStack)]
    stacks .= newStacks
    return word

-- | Gets the Program Counter
getPC :: State MachineState Word
getPC = do
  state <- get
  return $ fromJust $ state ^? registers . ix pcRegister

-- | Sets the program counter
setPC :: Word -> State MachineState ()
setPC addr = registers . ix pcRegister .= addr

-- | Fetches data from a DataLocation.
getData :: DataLocation -> State MachineState Word
getData (Constant word) = return word

getData (Register letter) = do
  regs <- use registers
  return $ if inRange registerBounds letter
           then regs ! letter
           else minWord

getData (MemoryLocation loc) = Memory.readWord <$> use memory <*> getData loc

getData (Stack l) = popStack l

getData (TapeIO letter) = do
    maybeTape <- use $ tapeDeck . at letter
    case maybeTape of
        Just tape -> do
            let (val, newtape) = runState (Tapes.tapeRead) tape
            tapeDeck . at letter .= Just newtape
            return val
        Nothing -> return minWord

getData (Io selector) = do
  char <- popInBuffer
  case char >>= Io.charToInternal of
    Just c -> return $ c
    Nothing -> return $ maxWord

getData (Negated loc) = negateWord <$> getData loc
getData (Incremented loc) = offset <$> getData loc <*> pure 1
getData (Decremented loc) = offset <$> getData loc <*> pure (-1)
getData (TimesFour loc) = mulWord <$> getData loc <*> pure (toWord 4)
getData (PlusFour loc) = offset <$> getData loc <*> pure 4
getData (FirstLetter loc)  = (extendToWord . view firstLetter)  <$> getData loc
getData (SecondLetter loc) = (extendToWord . view secondLetter) <$> getData loc
getData (ThirdLetter loc)  = (extendToWord . view thirdLetter)  <$> getData loc
getData (FourthLetter loc) = (extendToWord . view fourthLetter) <$> getData loc


-- | Applies a data write to any location, be it a register, main memory, etc.
setData :: DataLocation -> Word -> State MachineState ()
setData (Constant const) word = return () -- No-op for now. Raise interrupt later.
setData (Register letter) word = setRegister letter word

setData (MemoryLocation loc) word = flip setMemory word =<< (getData loc)

setData (Stack letter) word = pushStack letter word

setData (TapeIO letter) word = tapeDeck . at letter %= trywrite word
    where trywrite word (Just tape) = Just $ execState (Tapes.tapeWrite word) tape
          trywrite word Nothing = Nothing

setData (Io selector) word =
    action .= (IOWrite [word])

setData (Negated loc) word =
    setData loc (negateWord word)

setData (Incremented loc) word =
    setData loc (offset word 1)

setData (Decremented loc) word =
    setData loc (offset word (-1))

setData (TimesFour loc) word =
    setData loc (mulWord word (toWord 4))

setData (PlusFour loc) word =
    setData loc (offset word 4)

setData (FirstLetter loc) word =
    setData loc (extendToWord . view firstLetter $ word)

setData (SecondLetter loc) word =
    setData loc (extendToWord . view secondLetter $ word)

setData (ThirdLetter loc) word =
    setData loc (extendToWord . view thirdLetter $ word)

setData (FourthLetter loc) word =
    setData loc (extendToWord . view fourthLetter $ word)

bifunction :: (Word -> Word -> Word) -> DataLocation -> DataLocation -> DataLocation -> State MachineState ()
bifunction f src1 src2 dest =
    setData dest =<< f <$> getData src1 <*> getData src2

jumpCompare :: (Word -> Word -> Bool) -> DataLocation -> DataLocation -> DataLocation -> State MachineState ()
jumpCompare f src1 src2 jumpdest = do
    dat1 <- getData src1
    dat2 <- getData src2
    when (f dat1 dat2) (setPC =<< getData jumpdest)

-- | Applies an instruction to the state of the Machine.
runInstruction :: Instruction -> State MachineState ()
runInstruction Nop = return ()
runInstruction Instruction.Halt = action .= HaltAction
runInstruction (Move src dest) =
    setData dest =<< getData src

runInstruction (Add src1 src2 dest) = bifunction addWord src1 src2 dest
runInstruction (Sub src1 src2 dest) = bifunction subWord src1 src2 dest
runInstruction (Mul src1 src2 dest) = bifunction mulWord src1 src2 dest
runInstruction (Div src1 src2 dest) = bifunction divWord src1 src2 dest
runInstruction (Modulo src1 src2 dest) = bifunction modWord src1 src2 dest
runInstruction (And src1 src2 dest) = bifunction andWord src1 src2 dest

runInstruction (Jump dest) =
    setRegister pcRegister =<< getData dest

runInstruction (JumpZero datloc dest) = do
    dat <- getData datloc
    when (dat == minWord) (setPC =<< getData dest)

runInstruction (JumpEqual dat1 dat2 dest) = jumpCompare (==) dat1 dat2 dest
runInstruction (JumpNotEqual dat1 dat2 dest) = jumpCompare (/=) dat1 dat2 dest
runInstruction (JumpGreater dat1 dat2 dest) = jumpCompare (>) dat1 dat2 dest
runInstruction (JumpLesser dat1 dat2 dest) = jumpCompare (<) dat1 dat2 dest

runInstruction (FCall addr args) = do
    pushStack returnAddressStackId =<< getPC
    sequence . map (\arg -> (pushStack argStackId) =<< getData arg) $ args
    setPC =<< getData addr

runInstruction (Return args) = do
    -- It's important that Return reads the args first, THEN the
    -- PC, and THEN pushes the args on. That way it's possible to return
    -- items on the stack.
    argDatas <- sequence . map getData $ args
    setPC =<< popStack returnAddressStackId
    sequence_ . map (pushStack returnValueStackId) $ argDatas

runInstruction (Swap a b) = do
    -- Note that the order is important here.
    -- we definitely need to get the data before we set it.
    aData <- getData a
    bData <- getData b
    setData a bData
    setData b aData

runInstruction (PushAll dest args) = do
    argDatas <- sequence . map getData $ args
    sequence_ . map (setData dest) $ argDatas

runInstruction (PullAll source args) = pullAll source args
    where pullAll _ [] = return ()
          pullAll source (arg:xs) = do
              setData arg =<< getData source
              pullAll source xs

runInstruction (TapeSeek tape distance) = do
    tapeLetter <- view fourthLetter <$> getData tape
    distance <- getData distance
    tapeDeck . at tapeLetter %= fmap (execState (Tapes.tapeSeekForward distance))

runInstruction (TapeSeekBackwards tape distance) = do
    tapeLetter <- view fourthLetter <$> getData tape
    distance <- getData distance
    tapeDeck . at tapeLetter %= fmap (execState (Tapes.tapeSeekBackwards distance))

runInstruction (TapeRewind tapeIDloc) = do
    tapeID <- getData tapeIDloc
    let tapeLetter = tapeID ^. fourthLetter
    --let tryrewind (Just tape) = Just $ execState Tapes.tapeRewind tape
    -- "zoom" would probably work here, but the Maybe causes trouble.
    --zoom (tapeDeck . at tapeLetter) (Tapes.tapeRewind)
    --tapeDeck . at tapeLetter %= fmap (execState Tapes.tapeRewind)

tick :: State MachineState ()
tick = do
  pc <- getPC
  state <- get
  tickNum += 1
  let instructionResult = readInstruction pc mem
      mem = state ^. memory

  case instructionResult of
    Left reason -> trace ("BAD INSTRUCTION: " ++ show reason) $ do
        action .= HaltAction
    Right (InstructionParseResult instruction length) ->
        do
          setPC $ offsetBy pc length
          runInstruction instruction

start :: Int -> StateT MachineState IO ()
start ticktime = do
    lift $ hSetBuffering stdin NoBuffering
    lift $ hSetEcho stdin False
    run ticktime

run :: Int -> StateT MachineState IO ()
run ticktime = do
  input <- lift $ Io.readToBuffer []
  inBuffer <>= input

  hoistState tick -- Run the tick
  case ticktime of
    0 -> return ()
    n -> lift $ threadDelay n
  state <- hoistState $ get
  let currentAction = view action state
  --let ticknum = view tickNum state
  action .= NoAction -- Clear action
  case currentAction of
    NoAction -> run ticktime
    HaltAction -> return ()
    IOWrite charcodes -> do
        lift $ sequence (map Io.printChar charcodes)
        run ticktime
