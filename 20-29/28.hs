-- Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:
--
-- 21 22 23 24 25
-- 20  7  8  9 10
-- 19  6  1  2 11
-- 18  5  4  3 12
-- 17 16 15 14 13
--
-- It can be verified that the sum of the numbers on the diagonals is 101.
--
-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

steps :: [Integer]
steps = 1 : (steps' 2 0)
    where steps' n 4 = steps' (n + 2) 0
          steps' n c = n : steps' n (c + 1)

spiral :: Integer -> (Integer, Integer)
spiral n = let delta = (takeWhile (< n) steps) in
               foldl (\(x, y) z -> (x + z, y + x + z)) (0, 0) delta

main = print $ spiral 1001
