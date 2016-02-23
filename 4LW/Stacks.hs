module Stacks (Stack, Stacks, emptyStacks, emptyStack, push, pop, peekAll, size) where

import Prelude hiding (Word)
import Base27

import Data.Array

-- | The size limit on a stack.
limit :: Int
limit = 27^3

-- | A size-limited stack of words. The Int is the number of elements
-- currently in the stack.
-- Something more type-safe would be better, but the constructor
-- will be hidden anyway.
data SizeLimitedStack = SizeLimitedStack Int [Word]

type Stack = SizeLimitedStack

-- An array of all the stacks that a 4LW machine has available to it.
type Stacks = Array Letter Stack

instance Show SizeLimitedStack where
    show (SizeLimitedStack size words) = "stack " ++ show words

emptyStacks :: Stacks
emptyStacks = listArray (letter '_', letter 'Z') (repeat emptyStack)

emptyStack :: Stack
emptyStack = SizeLimitedStack 0 []

-- | Push a value onto a stack, failing if the stack is full.
push :: Stack -> Word -> Maybe Stack
push (SizeLimitedStack size s) word
    | size < limit = Just $ SizeLimitedStack (size + 1) (word:s)
    | otherwise = Nothing

-- | Pop a value from a stack, failing if the stack is empty.
pop :: Stack -> Maybe (Word, Stack)
pop (SizeLimitedStack _ []) = Nothing
pop (SizeLimitedStack size (x:xs)) = Just (x, SizeLimitedStack (size - 1) xs)

-- | Checks the number of elements in a stack.
size :: Stack -> Int
size (SizeLimitedStack size _) = size

-- | Returns a list of all elements in the stack.
-- Mostly intended for debugging purposes.
peekAll :: Stack -> [Word]
peekAll (SizeLimitedStack _ s) = s
