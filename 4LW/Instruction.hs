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
    MemoryLocation DataLocation |      -- A location in main memory
    Negated DataLocation |     -- The real result but negated
    Incremented DataLocation | -- The real result but incremented
    Decremented DataLocation |  -- The real result but decremented
    TimesFour DataLocation |
    FirstLetter DataLocation |
    SecondLetter DataLocation |
    ThirdLetter DataLocation |
    FourthLetter DataLocation
    deriving (Show, Eq)

-- | An Instruction is an action that the machine can perform.
data Instruction =
    Nop |
    Halt |
    Move DataLocation DataLocation |
    Add DataLocation DataLocation DataLocation |
    Sub DataLocation DataLocation DataLocation |
    Mul DataLocation DataLocation DataLocation |
    Div DataLocation DataLocation DataLocation |
    Modulo DataLocation DataLocation DataLocation |
    Jump DataLocation |
    JumpZero DataLocation DataLocation |
    JumpEqual DataLocation DataLocation DataLocation |
    JumpNotEqual DataLocation DataLocation DataLocation |
    FCall DataLocation [DataLocation] |
    Return [DataLocation] |
    Swap DataLocation DataLocation |
    PushAll DataLocation [DataLocation] |
    PullAll DataLocation [DataLocation]

    deriving (Show, Eq)


data BadInstruction =
     BadInstruction | BadOpcode | BadOperands deriving Show

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

-- | readInstruction will attempt to build an entire Instruction out of the address
-- and memory region supplied to it.
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
assembleRaw [] = Left BadInstruction
assembleRaw (opWord:rawOperands) = RawInstruction <$> pure opcode <*> operands
    where opcode = (opWord ^. firstLetter, opWord ^. secondLetter)
          operands = toEither BadOperands $ parseOperands rawOperands

-- Reads the words of an instruction, using the length flag.
readInstructionWords :: Word -> Memory.Memory -> [Word]
readInstructionWords addr mem = Memory.readWords mem addr len
    where lengthOffset = LetterLength 2
          len = WordLength . Base27.getValue $ Memory.readLetter mem (offsetBy addr lengthOffset)
          -- realLen = if len == WordLength 0 then WordLength 1 else len

-- | Takes a RawInstruction and figures out what it really is.
-- the resulting Instruction will be actually usable by 4LW.
constructInstruction :: RawInstruction -> Either BadInstruction Instruction
constructInstruction (RawInstruction opcode operands)
    | opcode == letter2 "__" = Right Nop
    | opcode == letter2 "HL" = Right Halt
    | opcode == letter2 "AD" = toBad $ Add <$>
                                (operands ^? ix 0) <*>
                                (operands ^? ix 1) <*>
                                (operands ^? ix 2)

    | opcode == letter2 "SB" = toBad $ Sub <$>
                                (operands ^? ix 0) <*>
                                (operands ^? ix 1) <*>
                                (operands ^? ix 2)

    | opcode == letter2 "ML" = toBad $ Mul <$>
                                 (operands ^? ix 0) <*>
                                 (operands ^? ix 1) <*>
                                 (operands ^? ix 2)

    | opcode == letter2 "DV" = toBad $ Div <$>
                                 (operands ^? ix 0) <*>
                                 (operands ^? ix 1) <*>
                                 (operands ^? ix 2)

    | opcode == letter2 "MD" = toBad $ Modulo <$>
                                 (operands ^? ix 0) <*>
                                 (operands ^? ix 1) <*>
                                 (operands ^? ix 2)

    | opcode == letter2 "MV" = toBad $ Move <$>
                                 (operands ^? ix 0) <*>
                                 (operands ^? ix 1)

    | opcode == letter2 "JP" = toBad $ Jump <$>
                                 (operands ^? ix 0)

    | opcode == letter2 "JZ" = toBad $ JumpZero <$>
                                 (operands ^? ix 0) <*>
                                 (operands ^? ix 1)

    | opcode == letter2 "JE" = toBad $ JumpEqual <$>
                                 (operands ^? ix 0) <*>
                                 (operands ^? ix 1) <*>
                                 (operands ^? ix 2)

    | opcode == letter2 "JN" = toBad $ JumpNotEqual <$>
                                 (operands ^? ix 0) <*>
                                 (operands ^? ix 1) <*>
                                 (operands ^? ix 2)

    | opcode == letter2 "FN" = toBad $ FCall <$>
                                 (operands ^? ix 0) <*>
                                 (pure . tail $ operands)

    | opcode == letter2 "RT" = Right $ Return operands

    | opcode == letter2 "SW" = toBad $ Swap <$>
                                 (operands ^? ix 0) <*>
                                 (operands ^? ix 1)

    | opcode == letter2 "PU" = toBad $ PushAll <$>
                                 (operands ^? ix 0) <*>
                                 (pure . tail $ operands)

    | opcode == letter2 "PL" = toBad $ PullAll <$>
                                 (operands ^? ix 0) <*>
                                 (pure . tail $ operands)

    | otherwise = Left BadOpcode


-- | Given the list of words that make up the operands (arguments) to an
--  instruction, turn them into real DataLocations.
parseOperands :: [Word] -> Maybe [DataLocation]
parseOperands words = reverse <$> parseOperands_ words []

parseOperands_ :: [Word] -> Operands -> Maybe [DataLocation]
parseOperands_ ((Word _ flag1 flag2 control):opdata:xs) ops
    | control == letter 'R' = build (Register (opdata ^. fourthLetter))
    -- | control == letter 'M' =  parseOperands_ xs (applyFlag flag (MemoryLocation opdata): ops)
    | control == letter 'C' = build (Constant opdata)
    | control == letter 'I' = build (Io (opdata ^. fourthLetter))
    | control == letter 'S' = build (Stack (opdata ^. fourthLetter))
    where build instruction = parseOperands_ xs (applyFlags [flag1, flag2] instruction:ops)

parseOperands_ (x:xs) ops = Nothing -- Odd number of words.
parseOperands_ [] ops = return ops

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
    | otherwise = error "Bad flag!"

-- | Applies multiple DataLocation flags, specified by letter.
applyFlags :: [Letter] -> DataLocation -> DataLocation
applyFlags letters loc = foldr applyFlag loc letters
