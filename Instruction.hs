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
    Move DataLocation DataLocation |
    Add DataLocation DataLocation DataLocation |
    Jump DataLocation
    deriving (Show, Eq)


data BadInstruction = BadInstruction deriving Show
data InstructionParseResult = InstructionParseResult RawInstruction Int

data RawInstruction = RawInstruction (Letter, Letter) Int Operands
                      deriving (Show, Eq)

type Operands = [DataLocation]


toEither :: a -> Maybe b -> Either a b
toEither leftValue = maybe (Left leftValue) Right

unwrapEither :: Either a b -> b
unwrapEither (Right b) = b
unwrapEither (Left a) = error "Failed Either"
                        
parseInstruction :: Word -> Memory.Memory -> Either BadInstruction InstructionParseResult
parseInstruction addr mem = Right $ InstructionParseResult rawInstruction instructionLength
    where lengthOffset = 2
          operandsOffset = 4
          opcode = (unwrapEither $ Memory.readLetter mem addr, unwrapEither $ Memory.readLetter mem (offset addr 1))
          instructionLength =
              getValue $ unwrapEither $ Memory.readLetter mem (offset addr lengthOffset) 
          rawOperands = unwrapEither $  Memory.readWords mem (offset addr operandsOffset) (instructionLength - 1) 
          operands = parseOperands rawOperands
          rawInstruction = RawInstruction opcode instructionLength (fromJust operands)

constructInstruction :: RawInstruction -> Either BadInstruction Instruction
constructInstruction (RawInstruction opcode len operands)
    | opcode == (letter2 "AD") = toEither BadInstruction $ Add <$>
                                 (operands ^? element 0) <*>
                                 (operands ^? element 1) <*>
                                 (operands ^? element 2)

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
    
