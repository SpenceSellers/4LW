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

data RawInstruction = RawInstruction (Word, Word) Int [Word]
                      deriving (Show, Eq)

type Operands = [DataLocation]
parseInstruction :: Word -> Memory.Memory -> Either BadInstruction InstructionParseResult
parseInstruction addr mem = undefined
    where lengthOffset = 2
          opcode = (Memory.readLetter mem addr, Memory.readLetter mem (offset addr 1))
          instructionLength = getValue $ Memory.readLetter mem (offset addr lengthOffset)

constructInstruction :: RawInstruction -> Either BadInstruction Instruction
constructInstruction (RawInstruction opcode len operands) = undefined
          
parseOperands :: [Word] -> [DataLocation]
parseOperands words = parseOperands_ words

parseOperands_ :: [Word] -> [DataLocation]
parseOperands_ (x:xs) = undefined
