{-# LANGUAGE TemplateHaskell #-}

module Machine where
import Data.Array
import Instruction
import Base27
import Control.Lens
import Control.Lens.At
import Control.Lens.Iso
import Control.Monad
import Data.Maybe
import Control.Monad.State.Lazy

stackRegister :: Letter
stackRegister = Letter 'S'

pcRegister :: Letter
pcRegister = Letter 'T'


type Registers = Array Letter Word

type Memory = Array Word Letter
    
data MachineState = MachineState {
      _registers :: Registers,
      _memory :: Memory
    } deriving (Show)
                  
makeLenses ''MachineState

blankMemory :: Memory
blankMemory = listArray (minWord, maxWord) (repeat (Letter '_'))

blankRegisters :: Registers
blankRegisters = listArray (Letter 'A', Letter 'T') (repeat (minWord))
                 
blankState :: MachineState
blankState = MachineState blankRegisters blankMemory

getPC :: State MachineState Word
getPC = do
  state <- get
  return $ fromJust $ state ^? registers.(ix pcRegister)

setPC :: Word -> State MachineState ()
setPC addr = registers.(ix pcRegister) .= addr
  
-- advance :: State MachineState ()
-- advance = 
tick :: State MachineState ()
tick = do
  registers.(ix (Letter 'A')) .= maxWord
  return ()

-- runInstruction :: Instruction -> State MachineState ()
-- runInstruction = 
  
