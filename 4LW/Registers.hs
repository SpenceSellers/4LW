module Registers where

import Prelude hiding (Word)
import Base27

import Data.Array
--import Data.Array.Lens
import Data.Ix

import Control.Lens


type Registers = Array Letter Word

stackRegister :: Letter
stackRegister = letter 'S'

pcRegister :: Letter
pcRegister = letter 'T'

registerBounds :: (Letter, Letter)
registerBounds = (letter 'A', letter 'T')

-- | A set of blank registers.
blankRegisters :: Registers
blankRegisters = listArray registerBounds (repeat minWord)

updateRegister :: Registers -> Letter -> Word -> Maybe Registers
updateRegister regs l value
    | inRange registerBounds l = Just $ regs // [(l, value)]
    | otherwise = Nothing
