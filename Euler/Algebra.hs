module Euler.Algebra
( factorize
, factorial
, factorial'
) where

import Euler.Sequence (primes)
import Control.Applicative ((<*>))

-- Takes a number and returns the factors of said number. Negative
-- numbers return all factors and their negative counterparts

factorize :: Integer -> [Integer]
factorize n 
    | n < 0     = [id, negate] <*> factorize (abs n)
    | n <= 1    = [n]
    | otherwise = factorize' n
    where factorize' n 
              | n == 1    = []
              | otherwise = f : factorize' (n `div` f)
              where f = head [x | x <- primes, n `mod` x == 0]


-- Basic Factorial
-- This differs from falling factorial which will decrease by
-- a certain amount.

factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n-1)


-- Falling Factorial
-- Pursues the factorial procedure but stops after a given number
-- of steps.

factorial' :: Integer -> Integer -> Integer
factorial' 1 _ = 1
factorial' _ 0 = 1 
factorial' n m = n * factorial' (n-1) (m-1)
