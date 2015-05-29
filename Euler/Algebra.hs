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


-- Returns the divisors of a given number, based on its factorization.
--
-- The power of some prime p, lets call it n, represents n possible divisors of a number 
-- (p, p*p, p*p*p, ...). Thus if we consider r_1, r_2, ..., r_n powers of primes p_1, p_2, 
-- ..., p_n, and also include the possibility of the 0th power of p_i for some 1 <= i <= n, 
-- the number of divisors is simply (r_1 + 1) * (r_2 + 1) * ... * (r_n + 1). We can 
-- generate the divisors similarly.

divisors :: Integer -> [Integer]
divisors n = divisors' (factorize n)
    where divisors' 


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
