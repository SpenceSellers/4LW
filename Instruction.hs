module Instruction where

import Base27
import qualified Memory
    
data DataLocation =
    Register Letter |
    MemoryLocation Word |
    Constant Word
    deriving (Show, Eq)

data Instruction =
    Move DataLocation DataLocation |
    Add DataLocation DataLocation DataLocation
    deriving (Show, Eq)


data BadInstruction = BadInstruction
data InstructionParseResult = InstructionParseResult Instruction Int


parseInstruction :: Word -> Memory.Memory -> Either BadInstruction InstructionParseResult
parseInstruction addr mem = undefined
    where lengthOffset = toWord 2
          opcode = (Memory.readLetter mem addr, Memory.readLetter mem (offset addr 1))
          
