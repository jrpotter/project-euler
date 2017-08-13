-- We shall say that an n-digit number is pandigital if it makes use of all the
-- digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1
-- through 5 pandigital.
--
-- The product 7254 is unusual, as the identity, 39 * 186 = 7254, containing
-- multiplicand, multiplier, and product is 1 through 9 pandigital. Find
-- the sum of all products whose multiplicand/multiplier/product identity can
-- be written as a 1 through 9 pandigital.
--
-- HINT: Some products can be obtained in more than one way so be sure to
-- only include it once in your sum.

import Data.List

-- First we filter our list into multiplicand/multiplier pairs that can possibly
-- satisfy the requirement of 9 unique digits. Thus we can ignore any pairs that
-- could not yield 9 total digits or any pairs that would yield a total number
-- of digits greater than 9. Afterward we ignore any combinations that yield a 0
-- anywhere in the multiplication equation.
--
-- Multiplicand (Digit Count) | Multiplier (Digit Count) | Product (Required)
-- 2                          | 3                        | 4
-- 3                          | 2                        | 4
-- 4                          | 1                        | 4
--
-- Its also unnecessary to consider both (2, 3) and (3, 2).

candidates :: [(Int, Int, Int)]
candidates = do
  c <- [3, 4]
  a <- withDigitCount c
  b <- withDigitCount (5 - c)
  return (a, b, a * b)
  where withDigitCount :: Int -> [Int]
        withDigitCount count = [10 ^ (count - 1) .. (10 ^ count) - 1]

isPandigital :: (Int, Int, Int) -> Bool
isPandigital (x, y, z) = let concat = show x ++ show y ++ show z in
                         length concat == 9 && (length . nub) concat == 9 && not ('0' `elem` concat)

main :: IO ()
main = print . sum . nub $ map (\(x, y, z) -> z) $ filter isPandigital candidates

