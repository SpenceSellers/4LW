module Stacks (Stack, Stacks, emptyStacks, emptyStack, push, pop) where

import Prelude hiding (Word)
import Base27

import Data.Array

limit :: Int
limit = 27^3

-- Something more type-safe would be better, but the constructor
-- will be hidden anyway.
data SizeLimitedStack = SizeLimitedStack Int [Word]


type Stack = SizeLimitedStack

type Stacks = Array Letter Stack

instance Show SizeLimitedStack where
    show (SizeLimitedStack size words) = "stack " ++ show words

emptyStacks :: Stacks
emptyStacks = listArray (letter '_', letter 'Z') (repeat emptyStack)

emptyStack :: Stack
emptyStack = SizeLimitedStack 0 []

push :: Stack -> Word -> Maybe Stack
push (SizeLimitedStack size s) word
    | size < limit = Just $ SizeLimitedStack (size + 1) (word:s)
    | otherwise = Nothing

pop :: Stack -> Maybe (Word, Stack)
pop (SizeLimitedStack _ []) = Nothing
pop (SizeLimitedStack size (x:xs)) = Just (x, SizeLimitedStack (size - 1) xs)

size :: Stack -> Int
size (SizeLimitedStack size _) = size

