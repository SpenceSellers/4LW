module Instruction where
import Data.Maybe
import Control.Lens
import Control.Applicative
import Debug.Trace
import Base27
import qualified Memory

data DataLocation =
    Register Letter |
    MemoryLocation Word |
    Constant Word
    deriving (Show, Eq)

data Instruction =
    Nop | 
    Move DataLocation DataLocation |
    Add DataLocation DataLocation DataLocation |
    Jump DataLocation
    deriving (Show, Eq)


data BadInstruction = BadInstruction deriving Show
data InstructionParseResult = InstructionParseResult Instruction Int

data RawInstruction = RawInstruction (Letter, Letter) Int Operands
                      deriving (Show, Eq)

type Operands = [DataLocation]


toEither :: a -> Maybe b -> Either a b
toEither leftValue = maybe (Left leftValue) Right

unwrapEither :: Either a b -> b
unwrapEither (Right b) = b
unwrapEither (Left a) = error "Failed Either"
                        
parseInstruction :: Word -> Memory.Memory -> Either BadInstruction InstructionParseResult
parseInstruction addr mem = Right $ InstructionParseResult instruction instructionLength
    where lengthOffset = 2
          operandsOffset = 4
          opcode = (unwrapEither $ Memory.readLetter mem addr, unwrapEither $ Memory.readLetter mem (offset addr 1))
          instructionLength =
              getValue $ unwrapEither $ Memory.readLetter mem (offset addr lengthOffset) 
          rawOperands = unwrapEither $  Memory.readWords mem (offset addr operandsOffset) (instructionLength - 1) 
          operands = parseOperands rawOperands
          rawInstruction = RawInstruction opcode instructionLength (fromJust operands)
          instruction = unwrapEither $ constructInstruction rawInstruction

constructInstruction :: RawInstruction -> Either BadInstruction Instruction
constructInstruction (RawInstruction opcode len operands)
    | opcode == (letter2 "__") = Right Nop
    | opcode == (letter2 "AD") = toEither BadInstruction $ Add <$>
                                 (operands ^? ix 0) <*>
                                 (operands ^? ix 1) <*>
                                 (operands ^? ix 2)
                                 
    | opcode == (letter2 "MV") = toEither BadInstruction $ Move <$>
                                 (operands ^? ix 0) <*>
                                 (operands ^? ix 1)
    -- | opcode == (letter2 "JP") 

-- TODO: Add error handling.
parseOperands :: [Word] -> Maybe [DataLocation]
parseOperands words = reverse <$> parseOperands_ words []

parseOperands_ :: [Word] -> Operands -> Maybe [DataLocation]
parseOperands_ (control:opdata:xs) ops
    | control == wrd "___R" =  parseOperands_ xs ((Register (lastLetter opdata)): ops)
    | control == wrd "___M" =  parseOperands_ xs ((MemoryLocation opdata): ops)
    | control == wrd "___C" =  parseOperands_ xs ((Constant opdata): ops)

parseOperands_ (x:xs) ops = Nothing -- Odd number of words.
parseOperands_ [] ops = return ops
    
-- readInstruction :: Word -> Memory.Memory -> Either BadInstruction Instruction
-- readInstruction addr mem =
--     constructInstruction =<< (parseInstruction addr mem)
