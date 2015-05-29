module Iteration
( permutations
, combinations
) where

import Euler.List (remove)

-- Returns all possible permutations of a list.

permuations :: [a] -> [[a]]
permutations [] = []
permutations (x:xs) = [[x]]
