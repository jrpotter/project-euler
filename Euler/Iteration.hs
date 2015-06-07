module Euler.Iteration
( combinations
) where

combinations :: [a] -> [[a]]
combinations [] = [[]]
combinations (x:xs) = [] ++ let ys = combinations xs in map (x:) ys ++ ys
