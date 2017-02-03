-- | Instruction.hs defines and parses instructions.
-- As part of this, it also defines and parses the arguments of Instructions,
-- DataLocations.

{-# LANGUAGE GADTs #-}
module Instruction where
import Prelude hiding (Word)
import Data.Maybe
import Control.Lens
import Control.Applicative
import Debug.Trace
import Base27
import Lengths
import qualified Memory

-- | A DataLocation is a place an instruction can place data or take it from.
-- These are usually supplied in the form of operands (instruction arguments).
-- Some DataLocations are modifications of other DataLocations.
data DataLocation =
    Register Letter |          -- A register
    Constant Word |            -- A fixed constant word
    Io Letter |                -- Std IO. Letter/Word will be used as a selector later.
    Stack Letter |
    TapeIO Letter |
    MemoryLocation DataLocation |      -- A location in main memory
    Negated DataLocation |     -- The real result but negated
    Incremented DataLocation | -- The real result but incremented
    Decremented DataLocation |  -- The real result but decremented
    TimesFour DataLocation |
    PlusFour DataLocation |
    FirstLetter DataLocation |
    SecondLetter DataLocation |
    ThirdLetter DataLocation |
    FourthLetter DataLocation
    deriving (Show, Eq)

-- | An Instruction is an action that the machine can perform.
-- What these instructions actually do is defined in Machine.hs
data Instruction =
    Nop |
    Halt |
    Move DataLocation DataLocation |
    Add DataLocation DataLocation DataLocation |
    Sub DataLocation DataLocation DataLocation |
    Mul DataLocation DataLocation DataLocation |
    Div DataLocation DataLocation DataLocation |
    Modulo DataLocation DataLocation DataLocation |
    And DataLocation DataLocation DataLocation |
    Jump DataLocation |
    JumpZero DataLocation DataLocation |
    JumpEqual DataLocation DataLocation DataLocation |
    JumpNotEqual DataLocation DataLocation DataLocation |
    JumpGreater DataLocation DataLocation DataLocation |
    JumpLesser DataLocation DataLocation DataLocation |
    FCall DataLocation [DataLocation] |
    Return [DataLocation] |
    Swap DataLocation DataLocation |
    Read DataLocation |
    PushAll DataLocation [DataLocation] |
    PullAll DataLocation [DataLocation] |
    TapeSeek DataLocation DataLocation  |
    TapeSeekBackwards DataLocation DataLocation |
    TapeRewind DataLocation |
    StackSize DataLocation DataLocation |
    SwapStacks DataLocation DataLocation

    deriving (Show, Eq)

-- | An error report of a 'failed' instruction parse.
data BadInstruction =
    BadInstruction 
    | BadOpcode (Letter, Letter) 
    | BadOperands BadOperand 
    | BadOperandsLength 
    | ZeroLengthInstruction 
    deriving Show

data BadOperand =
    BadOptype Letter 
    | BadOpcontrol Letter 
    deriving Show

-- | An InstructionParseResult is just the instruction, and how long
--  the instruction was, so we can change the Program Counter register
--  to the correct place for the next instruction.
data InstructionParseResult where
    InstructionParseResult :: Instruction -> LetterLength -> InstructionParseResult
    deriving (Show)

-- | A RawInstruction is all of the data that makes up an Instruction, but
--  it has not been assembled into something that 4LW can use yet.
--  It might turn out to not be a valid instruction at all.
data RawInstruction = RawInstruction (Letter, Letter) Operands
                      deriving (Show, Eq)

-- | Instruction operands are lists of DataLocations
type Operands = [DataLocation]

tr :: Show a => a -> a
tr x = trace (show x) x

-- Converts a Maybe to an Either, with a supplied error value.
toEither :: a -> Maybe b -> Either a b
toEither leftValue = maybe (Left leftValue) Right

-- | Converts an Either into a Maybe
toMaybe :: Either a b -> Maybe b
toMaybe (Right val) = Just val
toMaybe (Left _) = Nothing

-- | Converts an Either from one error type to another.
convertEither :: newerr -> Either e r -> Either newerr r
convertEither _ (Right r) = Right r
convertEither new (Left e) = Left new

toBad = toEither BadInstruction

toBadOpLen = toEither BadOperandsLength

badOperandsToBadInstruction :: Either BadOperand a -> Either BadInstruction a
badOperandsToBadInstruction (Left badop) = Left $ BadOperands badop
badOperandsToBadInstruction (Right val) = Right val

-- | readInstruction will attempt to build an entire Instruction out of the address
-- and memory supplied to it.
readInstruction :: Word -> Memory.Memory -> Either BadInstruction InstructionParseResult
readInstruction addr mem = InstructionParseResult <$> instruction <*> pure instructionLength
    where instructionWords = readInstructionWords addr mem
          instructionLength = toLetterLength . WordLength $ length instructionWords
          instruction = buildInstruction instructionWords

-- Builds a complete instruction out of the supplied Words.
buildInstruction :: [Word] -> Either BadInstruction Instruction
buildInstruction raw = constructInstruction =<< assembleRaw raw

-- Assembles a RawInstruction out of the supplied Words
assembleRaw :: [Word] -> Either BadInstruction RawInstruction
assembleRaw [] = Left ZeroLengthInstruction
assembleRaw (opWord:rawOperands) = RawInstruction <$> pure opcode <*> operands
    where opcode = (opWord ^. firstLetter, opWord ^. secondLetter)
          operands = badOperandsToBadInstruction $ parseOperands rawOperands

-- Reads the words of an instruction, using the length flag.
readInstructionWords :: Word -> Memory.Memory -> [Word]
readInstructionWords addr mem = Memory.readWords mem addr len
    where lengthOffset = LetterLength 2
          len = WordLength . Base27.getValue $ Memory.readLetter mem (offsetBy addr lengthOffset)
          -- realLen = if len == WordLength 0 then WordLength 1 else len

threeArgInstruction :: (DataLocation -> DataLocation -> DataLocation -> Instruction) -> Operands -> Either BadInstruction Instruction
threeArgInstruction inst operands = toBadOpLen $ inst <$>
    (operands ^? ix 0) <*>
    (operands ^? ix 1) <*>
    (operands ^? ix 2)

twoArgInstruction :: (DataLocation -> DataLocation -> Instruction) -> Operands -> Either BadInstruction Instruction
twoArgInstruction inst operands = toBadOpLen $ inst <$>
    (operands ^? ix 0) <*>
    (operands ^? ix 1)

oneArgInstruction :: (DataLocation -> Instruction) -> Operands -> Either BadInstruction Instruction
oneArgInstruction inst operands = toBadOpLen $ inst <$> (operands ^? ix 0)

-- | Takes a RawInstruction and figures out what it really is.
-- the resulting Instruction will be actually usable by 4LW.
constructInstruction :: RawInstruction -> Either BadInstruction Instruction
constructInstruction (RawInstruction opcode operands) =
    case charify opcode of
        ('_', '_') -> Right Nop
        ('H', 'L') -> Right Halt
        ('A', 'D') -> threeArgInstruction Add operands
        ('S', 'B') -> threeArgInstruction Sub operands
        ('M', 'L') -> threeArgInstruction Mul operands
        ('D', 'V') -> threeArgInstruction Div operands
        ('M', 'D') -> threeArgInstruction Modulo operands
        ('A', 'N') -> threeArgInstruction And operands
        ('M', 'V') -> twoArgInstruction Move operands

        ('J', 'P') -> oneArgInstruction Jump operands
        ('J', 'Z') -> twoArgInstruction JumpZero operands
        ('J', 'E') -> threeArgInstruction JumpEqual operands
        ('J', 'N') -> threeArgInstruction JumpNotEqual operands
        ('J', 'G') -> threeArgInstruction JumpGreater operands
        ('J', 'L') -> threeArgInstruction JumpLesser operands

        ('F', 'N') -> toBadOpLen $ FCall <$>
                                 (operands ^? ix 0) <*>
                                 (pure . tail $ operands)

        ('R', 'T') -> Right $ Return operands

        ('S', 'W') -> twoArgInstruction Swap operands
        ('R', 'D') -> oneArgInstruction Read operands

        ('P', 'U') -> toBadOpLen $ PushAll <$>
                                 (operands ^? ix 0) <*>
                                 (pure . tail $ operands)

        ('P', 'L') -> toBadOpLen $ PullAll <$>
                                 (operands ^? ix 0) <*>
                                 (pure . tail $ operands)

        ('T', 'S') -> twoArgInstruction TapeSeek operands
        ('T', 'B') -> twoArgInstruction TapeSeekBackwards operands
        ('T', 'R') -> oneArgInstruction TapeRewind operands

        ('S', 'S') -> twoArgInstruction StackSize operands

        ('W', 'S') -> twoArgInstruction SwapStacks operands

        (_, _) -> Left $ BadOpcode opcode

    where charify ((LetterV a), (LetterV b)) = (a, b)


-- | Given the list of words that make up the operands (arguments) to an
--  instruction, turn them into real DataLocations.
parseOperands :: [Word] -> Either BadOperand [DataLocation]
parseOperands words = reverse <$> parseOperands_ words []

parseOperands_ :: [Word] -> Operands -> Either BadOperand [DataLocation]
parseOperands_ ((Word optype flag1 flag2 control):rest) ops =
    case optype of
        LetterV '_' ->  case control of
            LetterV 'R' -> buildLong (Register (opdata ^. fourthLetter))
            LetterV 'C' -> buildLong (Constant opdata)
            LetterV 'I' -> buildLong (Io (opdata ^. fourthLetter))
            LetterV 'S' -> buildLong (Stack (opdata ^. fourthLetter))
            LetterV 'T' -> buildLong (TapeIO (opdata ^. fourthLetter))
            _ -> Left (BadOptype control)
            where (opdata : xs) = rest
                  buildLong instruction = parseOperands_ xs (applyFlags [flag1, flag2] instruction : ops)

        LetterV 'C' -> buildShort (Constant . extendToWord $ control)
        LetterV 'R' -> buildShort (Register control)
        LetterV 'S' -> buildShort (Stack control)
        LetterV 'I' -> buildShort (Io control)
        LetterV 'T' -> buildShort (TapeIO control)
        _ -> Left (BadOpcontrol optype)
    where buildShort loc = parseOperands_ rest ((applyFlags [flag1, flag2] loc ) : ops)

--parseOperands_ (x:xs) ops = Nothing -- Odd number of words.
parseOperands_ [] ops = Right ops

-- | Applies a single DataLocation flag, specified by letter.
applyFlag :: Letter -> DataLocation -> DataLocation
applyFlag flag loc
    | flag == letter '_' = loc
    | flag == letter 'N' = Negated loc
    | flag == letter 'M' = MemoryLocation loc
    | flag == letter 'I' = Incremented loc
    | flag == letter 'J' = Decremented loc
    | flag == letter 'F' = TimesFour loc
    | flag == letter 'A' = FirstLetter loc
    | flag == letter 'B' = SecondLetter loc
    | flag == letter 'C' = ThirdLetter loc
    | flag == letter 'D' = FourthLetter loc
    | flag == letter 'P' = PlusFour loc
    | otherwise = error "Bad flag!"

-- | Applies multiple DataLocation flags, specified by letter.
applyFlags :: [Letter] -> DataLocation -> DataLocation
applyFlags letters loc = foldr applyFlag loc letters

-- | Reads a sequence of instructions, starting at an address.
-- This is intended mostly for debugging purposes.
readSequence :: Memory.Memory -> Word -> [(Instruction, Word)]
readSequence mem addr = case result of
        Right (InstructionParseResult ins len) -> ((ins, addr) : readSequence mem (offsetBy addr len))
        Left err -> []
    where result = readInstruction addr mem
