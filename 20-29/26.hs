-- A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:
--
-- 1/2  = 0.5
-- 1/3  = 0.(3)
-- 1/4  = 0.25
-- 1/5  = 0.2
-- 1/6  = 0.1(6)
-- 1/7  = 0.(142857)
-- 1/8  = 0.125
-- 1/9  = 0.(1)
-- 1/10 = 0.1
-- Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
--
-- Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

import Control.Applicative
import qualified Data.Map.Strict as M

-- Returns all the divisors that form when performing long division
decimals :: Integer -> [Integer]
decimals 0 = undefined
decimals 1 = []
decimals x = decimals' 1 x
    where propagate n d = decimals' (10 * n) d
          decimals' n d
            | n == 0    = []
            | n < d     = propagate n d
            | otherwise = n : propagate (n `mod` d) d

-- Finds the indices of the first two elements that match
findMatch :: [Integer] -> (Maybe Integer, Maybe Integer)
findMatch [] = (Nothing, Nothing)
findMatch [x] = (Nothing, Nothing)
findMatch xs = findMatch' (M.empty) (zip xs [1..])
    where findMatch' m [] = (Nothing, Nothing)
          findMatch' m ((a, b):xs)
            | M.member a m = (Just b, M.lookup a m)
            | otherwise = findMatch' (M.insert a b m) xs

-- Returns the size of the repeating cycle present in the decimal expansion
difference :: Integer -> Maybe Integer
difference n = let (a, b) = findMatch $ decimals n
                in (-) <$> a <*> b

-- First index is number, second number is length of cycle
main = print $ foldl1 accum (zip indices diffs)
    where accum x y = if snd y > snd x then y else x
          indices = map Just [1..]
          diffs = map difference [1..1000]
