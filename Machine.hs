
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
getData :: MachineState -> DataLocation -> Word
getData state (Constant word) = word
getData state (Register letter) =
    if inRange registerBounds letter
    then (state^.registers) ! letter
    else minWord
getData state (MemoryLocation addr) = either (const minWord) (id) $ Memory.readWord (state^.memory) addr

-- | Applies a data write to any location, be it a register, main memory, etc.
setData :: MachineState -> DataLocation -> Word -> MachineState
setData state (Constant const) word = state -- No-op for now. Raise interrupt later.
                                      
setData state (Register letter) word =
    over registers (\regs -> regs  // [(letter, word)]) $ state
         
setData state (MemoryLocation addr) word =
    set memory (Memory.writeWord (state^.memory) addr word) $ state

-- | Applies an instruction to the state of the Machine.
runInstruction :: Instruction -> State MachineState ()
runInstruction (Move src dest) = do
  state <- get
  let datum = getData state src
  put $ setData state dest datum

runInstruction (Add src1 src2 dest) = do
  state <- get
  let data1 = getData state src1
      data2 = getData state src2
  put $ setData state dest (addWord data1 data2)
      
tick :: State MachineState ()
tick = do
  runInstruction (Move (Constant maxWord) (Register (Letter 'B')))
  runInstruction (Move (Constant maxWord)
                  (MemoryLocation
                   (Word (letter 'A', letter 'A', letter 'A', letter 'A'))))
  return ()

  
