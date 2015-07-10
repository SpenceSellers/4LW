module Instruction where
import Prelude hiding (Word)
import Data.Maybe
import Control.Lens
import Control.Applicative
import Debug.Trace
import Base27
import qualified Memory

data DataLocation =
    Register Letter |          -- A register
    Constant Word |            -- A fixed constant word
    Io Letter |                -- Std IO. Letter/Word will be used as a selector later.
    MemoryLocation DataLocation |      -- A location in main memory
    Negated DataLocation |     -- The real result but negated
    Incremented DataLocation | -- The real result but incremented
    Decremented DataLocation   -- The real result but decremented
    deriving (Show, Eq)

data Instruction =
    Nop |
    Halt |
    Move DataLocation DataLocation |
    Add DataLocation DataLocation DataLocation |
    Sub DataLocation DataLocation DataLocation |
    Mul DataLocation DataLocation DataLocation |
    Div DataLocation DataLocation DataLocation |
    Jump DataLocation |
    JumpZero DataLocation DataLocation

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

toBad = convertEither BadInstruction

readInstruction :: Word -> Memory.Memory -> Either BadInstruction InstructionParseResult
readInstruction addr mem = InstructionParseResult <$> instruction <*> toBad ((*4) <$> instructionLength )
    where lengthOffset = 2
          operandsOffset = 4
          opcode = (,) <$> Memory.readLetter mem addr <*> Memory.readLetter mem (offset addr 1)
          instructionLength = Base27.getValue <$> toBad (Memory.readLetter mem (offset addr lengthOffset))
          operands = do
            len <- instructionLength
            readOperands (offset addr operandsOffset) (len - 2) mem
          rawInstruction = RawInstruction <$> toBad opcode <*> instructionLength <*> operands
          instruction = constructInstruction =<< rawInstruction

constructInstruction :: RawInstruction -> Either BadInstruction Instruction
constructInstruction (RawInstruction opcode len operands)
    | opcode == letter2 "__" = Right Nop
    | opcode == letter2 "HL" = Right Halt
    | opcode == letter2 "AD" = toEither BadInstruction $ Add <$>
                                (operands ^? ix 0) <*>
                                (operands ^? ix 1) <*>
                                (operands ^? ix 2)

    | opcode == letter2 "SB" = toEither BadInstruction $ Sub <$>
                                (operands ^? ix 0) <*>
                                (operands ^? ix 1) <*>
                                (operands ^? ix 2)

    | opcode == letter2 "ML" = toEither BadInstruction $ Mul <$>
                                 (operands ^? ix 0) <*>
                                 (operands ^? ix 1) <*>
                                 (operands ^? ix 2)

    | opcode == letter2 "DV" = toEither BadInstruction $ Div <$>
                                 (operands ^? ix 0) <*>
                                 (operands ^? ix 1) <*>
                                 (operands ^? ix 2)

    | opcode == letter2 "MV" = toEither BadInstruction $ Move <$>
                                 (operands ^? ix 0) <*>
                                 (operands ^? ix 1)

    | opcode == letter2 "JP" = toEither BadInstruction $ Jump <$>
                                 (operands ^? ix 0)

    | opcode == letter2 "JZ" = toEither BadInstruction $ JumpZero <$>
                                 (operands ^? ix 0) <*>
                                 (operands ^? ix 1)

    | otherwise = trace ("Invalid OPcode is: " ++ (show opcode)) Left BadInstruction

readOperands :: Word -> Int -> Memory.Memory -> Either BadInstruction [DataLocation]
readOperands addr len mem =
    toEither BadInstruction $ parseOperands =<< toMaybe (Memory.readWords mem addr len)

parseOperands :: [Word] -> Maybe [DataLocation]
parseOperands words = reverse <$> parseOperands_ words []

parseOperands_ :: [Word] -> Operands -> Maybe [DataLocation]
parseOperands_ ((Word _ flag1 flag2 control):opdata:xs) ops
    | control == letter 'R' =  parseOperands_ xs (applyFlags [flag1, flag2] (Register (lastLetter opdata)): ops)
    -- | control == letter 'M' =  parseOperands_ xs (applyFlag flag (MemoryLocation opdata): ops)
    | control == letter 'C' =  parseOperands_ xs (applyFlags [flag1, flag2] (Constant opdata): ops)
    | control == letter 'I' =  parseOperands_ xs (applyFlags [flag1, flag2] (Io (lastLetter opdata)): ops)

parseOperands_ (x:xs) ops = Nothing -- Odd number of words.
parseOperands_ [] ops = return ops

applyFlag :: Letter -> DataLocation -> DataLocation
applyFlag flag loc
    | flag == letter '_' = loc
    | flag == letter 'N' = Negated loc
    | flag == letter 'M' = MemoryLocation loc
    | flag == letter 'I' = Incremented loc
    | flag == letter 'D' = Decremented loc
    | otherwise = error "Bad flag!"

applyFlags :: [Letter] -> DataLocation -> DataLocation
applyFlags letters loc = foldr applyFlag loc letters
