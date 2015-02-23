
{-# LANGUAGE TemplateHaskell #-}

module Machine where
import Data.Array
import Data.Ix
import Instruction
import Base27
import qualified Memory
import Control.Lens
import Control.Lens.At
import Control.Lens.Iso
import Control.Monad
import Data.Maybe
import Control.Monad.State.Lazy
import Control.Applicative
import Control.Monad.Reader
import Debug.Trace

hoistState :: Monad m => State s a -> StateT s m a
hoistState = StateT . (return .) . runState

stackRegister :: Letter
stackRegister = letter 'S'

pcRegister :: Letter
pcRegister = letter 'T'

data MachineAction = NoAction | Halt deriving (Show, Eq)

type Registers = Array Letter Word

-- | Stores the entire machine state from one instruction to the next.
data MachineState = MachineState {
      _registers :: Registers,
      _memory :: Memory.Memory
    } deriving (Show)
                  
makeLenses ''MachineState

registerBounds :: (Letter, Letter)
registerBounds = (letter 'A', letter 'T')

-- | A set of blank registers.
blankRegisters :: Registers
blankRegisters = listArray registerBounds (repeat (minWord))

-- | A blank "starting" state of the machine, with everything zeroed.
blankState :: MachineState
blankState = MachineState blankRegisters Memory.blankMemory

-- | Gets the Program Counter
getPC :: State MachineState Word
getPC = do
  state <- get
  return $ fromJust $ state ^? registers.(ix pcRegister)

-- | Sets the program counter
setPC :: Word -> State MachineState ()
setPC addr = registers.(ix pcRegister) .= addr

-- | Fetches data from a DataLocation.
getData :: DataLocation -> State MachineState Word
getData (Constant word) = return word
getData (Register letter) = do
  state <- get
  return $ if inRange registerBounds letter
           then (state^.registers) ! letter
           else minWord
                
getData (MemoryLocation addr) = do
  state <- get
  return $ either (const minWord) id $ Memory.readWord (state^.memory) addr

-- | Applies a data write to any location, be it a register, main memory, etc.
setData :: DataLocation -> Word -> State MachineState ()
setData (Constant const) word = return () -- No-op for now. Raise interrupt later.
setData (Register letter) word =
    registers %= (\regs -> regs  // [(letter, word)])
      
setData (MemoryLocation addr) word =
    memory %= \mem -> (Memory.writeWord mem addr word)

-- | Applies an instruction to the state of the Machine.
runInstruction :: Instruction -> State MachineState ()
runInstruction Nop = return ()
runInstruction (Move src dest) =
    setData dest =<< getData src

runInstruction (Add src1 src2 dest) =
    setData dest =<< addWord <$> getData src1 <*> getData src2

runInstruction (Jump dest) =
    setData (Register pcRegister) =<< getData dest
      
tick :: State MachineState MachineAction
tick = do
  pc <- getPC
  state <- get
  let instructionResult = parseInstruction pc mem
      mem = state ^. memory
            
  case instructionResult of
    Left BadInstruction -> return Halt
    Right (InstructionParseResult instruction length) ->
        do
          trace ("ins: " ++ (show instruction)) return ()
          setPC $ (offset pc length)
          runInstruction instruction
          return NoAction
          
run :: StateT MachineState IO ()
run = do
  state <- get
  tickResult <- hoistState $ tick
  registerA <- hoistState $ getData (Register (Letter 'A'))
  registerH <- hoistState $ getData (Register (Letter 'H'))
  liftIO $ putStrLn $ "Register A: " ++ (show $ registerA)
  case tickResult of
    NoAction -> run
    Halt -> return ()
