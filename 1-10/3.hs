-- The prime factors of 13195 are 5, 7, 13, and 29.
--
-- What is the largest prime factor of the number 600851475143

import Euler.Sequence (primes)

factors :: Integer -> [Integer]
factors n
    | n <= 1    = []
    | otherwise = f : factors (n `div` f)
    where f = head [x | x <- primes, n `mod` x == 0]

main = print . maximum $ factors 600851475143
