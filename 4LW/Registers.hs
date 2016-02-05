-- | Handles 4LW's registers.
-- Each register can hod one Word.
module Registers where

import Prelude hiding (Word)
import Base27

import Data.Array
import Data.Ix
import Control.Lens

-- | Holds all of the registers.
type Registers = Array Letter Word

-- | The letter ID of the PC register.
pcRegister :: Letter
pcRegister = letter 'T'

registerBounds :: (Letter, Letter)
registerBounds = (letter 'A', letter 'T')

-- | A set of blank registers.
blankRegisters :: Registers
blankRegisters = listArray registerBounds (repeat minWord)

-- |
updateRegister :: Registers -> Letter -> Word -> Maybe Registers
updateRegister regs l value
    | inRange registerBounds l = Just $ regs // [(l, value)]
    | otherwise = Nothing
