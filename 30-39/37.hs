-- The number 3797 has an interesting property. Being prime itself, it is
-- possible to continuously remove digits from left to right, and remain prime
-- at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left
-- 3797, 379, 37, and 3.
--
-- Find the sum of the only eleven primes that are both truncatable from left to
-- right and right to left.
--
-- NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

import qualified Data.Set as S
import Euler.Algebra (isPrime)
import Euler.Sequence (primes)

rightReduce :: Integer -> [Integer]
rightReduce 0 = []
rightReduce x = x : rightReduce (x `div` 10)

leftReduce :: Integer -> [Integer]
leftReduce x
  | x < 10 = x : []
  | otherwise = let y = floor $ logBase 10 (fromIntegral x)
                 in x : (leftReduce $ x `mod` 10^y)

truncatables :: Integer -> S.Set Integer -> [(Bool, Integer)]
truncatables x ps = truncatables' x ps S.empty
  where truncatables' x ps s = append x (if S.member x ps then S.insert x s else s)
        append x s
          | x < 10 = (False, x) : truncatables' (x + 1) ps s
          | otherwise = (all (\x -> S.member x s) (rightReduce x) &&
                         all (\x -> S.member x s) (leftReduce x), x) : truncatables' (x + 1) ps s

main = print . sum $ map snd $ take 11 $ filter (\x -> fst x) $ truncatables 1 primes'
  where primes' = S.fromList $ take 100000 primes

