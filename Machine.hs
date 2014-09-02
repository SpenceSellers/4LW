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
    
data MachineState = MachineState {
      _registers :: Registers,
      _memory :: Memory.Memory
    } deriving (Show)
                  
makeLenses ''MachineState

registerBounds :: (Letter, Letter)
registerBounds = (letter 'A', letter 'T')
                 
blankRegisters :: Registers
blankRegisters = listArray registerBounds (repeat (minWord))
                 
blankState :: MachineState
blankState = MachineState blankRegisters Memory.blankMemory

getPC :: State MachineState Word
getPC = do
  state <- get
  return $ fromJust $ state ^? registers.(ix pcRegister)

setPC :: Word -> State MachineState ()
setPC addr = registers.(ix pcRegister) .= addr


getData :: MachineState -> DataLocation -> Word
getData state (Constant word) = word
getData state (Register letter) =
    if inRange registerBounds letter
    then (state^.registers) ! letter
    else minWord
getData state (MemoryLocation addr) = Memory.readWord (state^.memory) addr

setData :: MachineState -> DataLocation -> Word -> MachineState
setData state (Constant const) word = state -- No-op for now. Raise interrupt later.
setData state (Register letter) word = over registers (\regs -> regs  // [(letter, word)]) $ state
setData state (MemoryLocation addr) word =
    set memory (Memory.writeWord (state^.memory) addr word) $ state



runInstruction :: Instruction -> State MachineState ()
runInstruction (Move src dest) = do
  state <- get
  let datum = getData state src
  put $ setData state dest datum
      
tick :: State MachineState ()
tick = do
  runInstruction (Move (Constant maxWord) (Register (Letter 'B')))
  return ()

  
