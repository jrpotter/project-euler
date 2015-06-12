-- A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4.
-- If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:
--
-- 012   021   102   120   201   210
--
-- What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

-- Rather than actual computing permutations, we can work down, knowing the first 9! permutations start
-- with a '0', the second 9! start with '1', etc.
-- Note the following is a permutation off, since the first permutation isn't considered until the very end

import Euler.List (remove)
import Euler.Algebra (factorial)

permute' :: Integer -> Integer -> [Integer]
permute' 0 _ = []
permute' n x = s : permute' (n - (f * s)) (x - 1)
    where f = factorial x
          s = n `div` f

removeSeq :: [Integer] -> [Integer] -> [Integer]
removeSeq [] ys = ys
removeSeq (x:xs) ys = case remove x ys of
                          (Nothing, zs) -> zs
                          (Just z, zs) -> z : removeSeq (xs) zs

main = print $ removeSeq (permute' 999999 9) [0..9]
