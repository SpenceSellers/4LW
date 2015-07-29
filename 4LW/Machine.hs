
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Machine where
import Prelude hiding (Word)
import System.IO
import Data.Array
import Data.Ix
import Instruction
import Base27
import Lengths
import qualified Memory
import qualified Io
import Control.Lens
import Control.Monad
import Data.Maybe
import Control.Monad.State.Lazy
import Control.Applicative
import Control.Monad.Reader
import Debug.Trace

-- | Brings a function into the State monad.
hoistState :: Monad m => State s a -> StateT s m a
hoistState = StateT . (return .) . runState

stackRegister :: Letter
stackRegister = letter 'S'

pcRegister :: Letter
pcRegister = letter 'T'

data MachineAction = NoAction |
                     HaltAction |
                     IOWrite String
                     deriving (Show, Eq)

type Registers = Array Letter Word

-- | Stores the entire machine state from one instruction to the next.
data MachineState = MachineState {
      _registers :: Registers,
      _memory :: Memory.Memory,
      _action :: MachineAction,
      _tickNum :: Integer,
      _inBuffer :: [Char],
      _outBuffer :: [Char]
    } deriving (Show)

makeLenses ''MachineState

registerBounds :: (Letter, Letter)
registerBounds = (letter 'A', letter 'T')

-- | A set of blank registers.
blankRegisters :: Registers
blankRegisters = listArray registerBounds (repeat minWord)

-- | A blank "starting" state of the machine, with everything zeroed.
blankState :: MachineState
blankState = MachineState blankRegisters Memory.blankMemory NoAction 0 [] []

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
setRegister r w = registers %= (\regs -> regs // [(r, w)])

setMemory :: Word -> Word -> State MachineState ()
setMemory addr word = memory %= \mem -> Memory.writeWord mem addr word

pushStack :: Word -> State MachineState ()
pushStack word = do
    regs <- use registers
    let oldStackAddr = regs ! stackRegister
    let newStackAddr = offsetBy oldStackAddr (WordLength (-1))
    setMemory newStackAddr word -- Make sure to use the NEW one.

    setRegister stackRegister newStackAddr

popStack :: State MachineState Word
popStack = do
    mem <- use memory
    regs <- use registers
    let stackAddr = regs ! stackRegister
    let newAddr = offsetBy stackAddr (WordLength 1)
    setRegister stackRegister newAddr
    return $ Memory.readWord mem stackAddr

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

getData (Stack) = popStack

getData (Io selector) = do
  char <- popInBuffer
  case char of
    Just c -> return $ Io.charToInternal c
    Nothing -> return $ maxWord

getData (Negated loc) = negateWord <$> getData loc

getData (Incremented loc) = offset <$> getData loc <*> pure 1

getData (Decremented loc) = offset <$> getData loc <*> pure (-1)

-- | Applies a data write to any location, be it a register, main memory, etc.
setData :: DataLocation -> Word -> State MachineState ()
setData (Constant const) word = return () -- No-op for now. Raise interrupt later.
setData (Register letter) word = setRegister letter word

setData (MemoryLocation loc) word = flip setMemory word =<< (getData loc)

setData (Stack) word = pushStack word

setData (Io selector) word =
    action .= IOWrite [Io.internalToChar word]

setData (Negated loc) word =
    setData loc (negateWord word)

setData (Incremented loc) word =
    setData loc (offset word 1)

setData (Decremented loc) word =
    setData loc (offset word (-1))

-- | Applies an instruction to the state of the Machine.
runInstruction :: Instruction -> State MachineState ()
runInstruction Nop = return ()
runInstruction Instruction.Halt = action .= HaltAction
runInstruction (Move src dest) =
    setData dest =<< getData src

runInstruction (Add src1 src2 dest) =
    setData dest =<< addWord <$> getData src1 <*> getData src2

runInstruction (Sub src1 src2 dest) =
    setData dest =<< subWord <$> getData src1 <*> getData src2

runInstruction (Mul src1 src2 dest) =
    setData dest =<< mulWord <$> getData src1 <*> getData src2

runInstruction (Div src1 src2 dest) =
    setData dest =<< divWord <$> getData src1 <*> getData src2

runInstruction (Jump dest) =
    setRegister pcRegister =<< getData dest

runInstruction (JumpZero datloc dest) = do
    dat <- getData datloc
    when (dat == minWord) (setRegister pcRegister =<< getData dest)

runInstruction (FCall addr args) = do
    pushStack =<< getPC
    sequence . map (\arg -> pushStack =<< getData arg) $ args
    setPC =<< getData addr

runInstruction (Return args) = do
    -- It's important that Return reads the args first, THEN the
    -- PC, and THEN pushes the args on. That way it's possible to return
    -- items on the stack.
    argDatas <- sequence . map getData $ args
    setPC =<< popStack
    sequence . map pushStack $ argDatas
    return ()


tick :: State MachineState ()
tick = do
  pc <- getPC
  state <- get
  tickNum += 1
  let instructionResult = readInstruction pc mem
      mem = state ^. memory

  case instructionResult of
    Left BadInstruction -> trace ("BAD INSTRUCTION") $ return ()
    Right (InstructionParseResult instruction length) ->
        do
          setPC $ offsetBy pc length
          runInstruction instruction

start :: StateT MachineState IO ()
start = do
    hoistState $ setRegister stackRegister (wrd "ZZZZ")
    lift $ hSetBuffering stdin NoBuffering
    run

run :: StateT MachineState IO ()
run = do
  input <- lift $ Io.readToBuffer []
  inBuffer <>= input

  hoistState tick -- Run the tick

  state <- hoistState $ get
  let currentAction = view action state
  --let ticknum = view tickNum state
  action .= NoAction -- Clear action

  case currentAction of
    NoAction -> run
    HaltAction -> return ()
    IOWrite str -> do
        lift $ putStr str
        lift $ hFlush stdout
        run