-- The number, 197, is called a circular prime because all rotations of the
-- digits: 197, 971, and 719, are themselves prime.
--
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71,
-- 73, 79, and 97.
--
-- How many circular primes are there below one million?

import qualified Data.Set as S

import Euler.Sequence (primes)

-- Constructs a set containing the rotated integers if they are circular.
isCircular :: Integer -> S.Set Integer -> S.Set Integer
isCircular x s = let y = floor $ logBase 10 (fromIntegral x)
                     ls = rotate x (y+1) y
                     p = all (\x -> S.member x s) ls
                  in if p then S.fromList ls else S.empty
  where rotate _ 0 _ = []
        rotate x n m = let y = 10^(m) * (x `mod` 10) + (x `div` 10)
                        in x : (rotate y (n-1) m)

-- Builds all circular primes up to a specified limit.
circular :: Integer -> S.Set Integer -> [Integer]
circular limit cache = circular' primes limit S.empty
  where circular' (p:ps) limit m
          | p >= limit = []
          | S.member p m = p : circular' ps limit m
          | otherwise = let c = isCircular p cache
                            rs = circular' ps limit (S.union c m)
                         in if S.member p c then p : rs else rs

main = print . length $ do
  let limit = 1000000
  circular limit (primes' limit)
  where primes' x = S.fromList $ takeWhile (<x) primes

