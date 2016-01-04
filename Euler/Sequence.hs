module Euler.Sequence
( fibonacci
, collatz
, primes
) where

import qualified Data.PQueue.Prio.Min as PQ


-- Fibonacci Sequence (0, 1, 1, 2, 3, 5, 8, ...)

fibonacci :: [Integer]
fibonacci = 0 : 1 : (fib 0 1)
    where fib x y = x + y : fib y (x + y)


-- Collatz Sequence
-- Takes a starting value, and returns all intermediate elements
-- between the starting value and 1

collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n = n : if even n then collatz (n `div` 2) else collatz (3 * n + 1)


-- Primes (The Sieve of Eratosthenes)
-- Described in https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf

sieve_E :: [Integer] -> [Integer]
sieve_E [] = []
sieve_E (x:xs) = x : sieve xs (insertprime x xs PQ.empty)
    where
        insertprime p xs table = PQ.insert (p*p) (map (*p) xs) table
        sieve [] table = []
        sieve (x:xs) table
            | nextComposite <= x = sieve xs (adjust table)
            | otherwise          = x : sieve xs (insertprime x xs table)
            where
                nextComposite = fst $ PQ.findMin table
                adjust table
                    | n <= x    = adjust (PQ.insert n' ns (PQ.deleteMin table))
                    | otherwise = table
                    where
                        (n, n':ns) = PQ.findMin table

primes :: [Integer]
primes = 2 : 3 : 5 : 7 : sieve_E (spin wheel 11)
    where spin (x:xs) n = n : spin xs (n + x)
          wheel = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8:
                  6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel
