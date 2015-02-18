
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

stackRegister :: Letter
stackRegister = letter 'S'

pcRegister :: Letter
pcRegister = letter 'T'


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
  return $ either (const minWord) (id) $ Memory.readWord (state^.memory) addr

-- | Applies a data write to any location, be it a register, main memory, etc.
setData :: DataLocation -> Word -> State MachineState ()
setData (Constant const) word = return () -- No-op for now. Raise interrupt later.
setData (Register letter) word =
    registers %= (\regs -> regs  // [(letter, word)])
      
setData (MemoryLocation addr) word =
  memory %= \mem -> (Memory.writeWord mem addr word)

-- | Applies an instruction to the state of the Machine.
runInstruction :: Instruction -> State MachineState ()
runInstruction (Move src dest) = do
  state <- get
  datum <- getData src
  setData dest datum

runInstruction (Add src1 src2 dest) = do
  state <- get
  data1 <- getData src1
  data2 <- getData src2
  setData dest (addWord data1 data2)
      
tick :: State MachineState ()
tick = do
  runInstruction (Move (Constant maxWord) (Register (Letter 'B')))
  runInstruction (Move (Constant maxWord)
                  (MemoryLocation
                   (Word (letter '_', letter '_', letter 'A', letter 'A'))))
  return ()

  
