-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
--
-- Find the sum of all numbers which are equal to the sum of the factorial of
-- their digits.
--
-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.

import qualified Data.Map as M
import Euler.Algebra

factorialMap :: M.Map Integer Integer
factorialMap = M.fromList $ map (\x -> (x, factorial x)) [0..9]

curious :: Integer -> Bool
curious x = Just x == curious' x factorialMap
  where curious' 0 _ = Just 0
        curious' x map = (+) <$> (M.lookup (x `mod` 10) map) <*> curious' (x `div` 10) map

-- We see that 9,999,999 > 7 * 9! meaning this is a natural upper limit.
main :: IO ()
main = print . sum $ filter curious [10..(7 * factorial 9)]
