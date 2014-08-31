module Instruction where

import Base27

data DataLocation =
    Register |
    MemoryLocation [Letter] |
    Constant [Letter]
    deriving (Show, Eq)

data Instruction =
    Move DataLocation DataLocation |
    Add DataLocation DataLocation DataLocation
    deriving (Show, Eq)
                 
