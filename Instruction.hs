module Instruction where
import Prelude hiding (Word)
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
                            deriving (Show)

data RawInstruction = RawInstruction (Letter, Letter) Int Operands
                      deriving (Show, Eq)

type Operands = [DataLocation]

tr :: Show a => a -> a
tr x = trace (show x) x
       
toEither :: a -> Maybe b -> Either a b
toEither leftValue = maybe (Left leftValue) Right

toMaybe :: Either a b -> Maybe b
toMaybe (Right val) = Just val
toMaybe (Left _) = Nothing

unwrapEither :: Either a b -> b
unwrapEither (Right b) = b
unwrapEither (Left a) = error "Failed Either"

convertEither :: newerr -> Either e r -> Either newerr r
convertEither _ (Right r) = Right r
convertEither new (Left e) = Left new
                             
parseInstruction :: Word -> Memory.Memory -> Either BadInstruction InstructionParseResult
parseInstruction addr mem = tr $ InstructionParseResult <$> instruction <*> toBad ((*4) <$> instructionLength )
    where lengthOffset = 2
          operandsOffset = 4
          opcode = (,) <$> (Memory.readLetter mem addr) <*> (Memory.readLetter mem (offset addr 1))
          instructionLength = getValue <$> Memory.readLetter mem (offset addr lengthOffset) 
          rawOperands =  do
            len <- instructionLength
            Memory.readWords mem (offset addr operandsOffset) (len - 2)
          operands = parseOperands =<< toMaybe (tr rawOperands)
          rawInstruction = RawInstruction <$> toBad opcode <*> toBad instructionLength <*> toEither BadInstruction operands
          instruction = constructInstruction =<< rawInstruction
          toBad = convertEither BadInstruction

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
    | opcode == (letter2 "JP") = toEither BadInstruction $ Jump <$>
                                 (operands ^? ix 0)
    | otherwise = trace ("OPcode is: " ++ (show opcode)) Left BadInstruction
    -- | opcode == (letter2 "JP") 

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
