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
    Stack |
    MemoryLocation DataLocation |      -- A location in main memory
    Negated DataLocation |     -- The real result but negated
    Incremented DataLocation | -- The real result but incremented
    Decremented DataLocation   -- The real result but decremented
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
    Jump DataLocation |
    JumpZero DataLocation DataLocation |
    FCall DataLocation [DataLocation] |
    Return [DataLocation]

    deriving (Show, Eq)


data BadInstruction =
     BadInstruction deriving Show

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

toBad = convertEither BadInstruction

-- | readInstruction will attempt to build an entire Instruction out of the address
-- and memory region supplied to it.
readInstruction :: Word -> Memory.Memory -> Either BadInstruction InstructionParseResult
readInstruction addr mem = InstructionParseResult <$> instruction <*> pure instructionLength
    where instructionWords = readInstructionWords addr mem
          instructionLength = (toLetterLength . WordLength $ length instructionWords)
          instruction = buildInstruction instructionWords

-- Builds a complete instruction out of the supplied Words.
buildInstruction :: [Word] -> Either BadInstruction Instruction
buildInstruction raw = constructInstruction =<< assembleRaw raw

-- Assembles a RawInstruction out of the supplied Words
assembleRaw :: [Word] -> Either BadInstruction RawInstruction
assembleRaw [] = Left BadInstruction
assembleRaw (opWord:rawOperands) = RawInstruction <$> pure opcode <*> operands
    where opcode = (opWord ^. firstLetter, opWord ^. secondLetter)
          operands = toEither BadInstruction $ parseOperands rawOperands

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
    | opcode == letter2 "FN" = toEither BadInstruction $ FCall <$>
                                 (operands ^? ix 0) <*>
                                 (pure . tail $ operands)
    | opcode == letter2 "RT" = Right $ Return operands

    | otherwise = trace ("Invalid OPcode is: " ++ (show opcode)) Left BadInstruction


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
    | control == letter 'S' = build Stack
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
    | flag == letter 'D' = Decremented loc
    | otherwise = error "Bad flag!"

-- | Applies multiple DataLocation flags, specified by letter.
applyFlags :: [Letter] -> DataLocation -> DataLocation
applyFlags letters loc = foldr applyFlag loc letters
