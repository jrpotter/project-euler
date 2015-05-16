module Sequence where

-- Fibonacci Sequence (0, 1, 1, 2, 3, 5, 8, ...)
fibonacci :: [Integer]
fibonacci = 0 : 1 : (fib 0 1)
    where fib x y = x + y : fib y (x + y)

-- Primes (note a genuine Sieve of Eratosthenes will eventually be implemented)
primes :: [Integer]
primes = let sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0] in sieve [2..]
