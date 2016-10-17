-- Surprisingly there are only three numbers that can be written as the sum of
-- fourth powers of their digits:
--
-- 1634 = 1^4 + 6^4 + 3^4 + 4^4
-- 8208 = 8^4 + 2^4 + 0^4 + 8^4
-- 9474 = 9^4 + 4^4 + 7^4 + 4^4
--
-- As 1 = 1^4 is not a sum it is not included.
-- The sum of these numbers is 1634 + 8208 + 9474 = 19316.
-- Find the sum of all the numbers that can be written as the sum of fifth
-- powers of their digits.

import qualified Data.Char as C

-- Find the sum of the power of the digits of the given number
digitSum :: Integer -> Integer -> Integer
digitSum n p = sum $ map (fromIntegral . flip (^) p . C.digitToInt) (show n)

-- Note this number does not actually succeed 10^6 so this is a hard limit
limit :: Integer
limit = 9 ^ 5 * 7

main = print . sum $ filter (\x -> digitSum x 5 == x) [10..limit]
