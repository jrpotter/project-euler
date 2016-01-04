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

import Data.List (elemIndex)

digits :: Integer -> [Integer]
digits t = digits' (ceiling $ log (fromIntegral t) / log 10)
    where digits' c = (10^c `div` t) `mod` 10 : digits' (c+1)

count :: [Integer] -> [Integer] -> Int -> Int
count (x:xs) ys z
  | x `elem` ys = case elemIndex x ys of
                    Just i -> length ys - i
                    _      -> 0
  | otherwise = count xs (ys ++ [x]) (z+1)

main = mapM_ print (map (\x -> take 30 $ digits x) [2..999]) --print $ foldl (\x y -> max' x y) (1, 1) [2..9999]
    where count' n = count (digits n) [] 0
          max' a@(n, c) i
              | count' i > c = (i, count' i)
              | otherwise   = a

