-- The fraction 49/98 is a curious fraction, as an inexperienced mathematician
-- in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which
-- is correct, is obtained by cancelling the 9s.
--
-- We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
--
-- There are exactly four non-trivial examples of this type of fraction, less
-- than one in value, and containing two digits in the numerator and
-- denominator.
--
-- If the product of these four fractions is given in its lowest common terms,
-- find the value of the denominator.

import Control.Monad
import Data.List
import Euler.Algebra

simplify' :: Integer -> Integer -> (Integer, Integer)
simplify' a b = let num = (show a) \\ (show b)
                    den = (show b) \\ (show a)
                in process num den
  where process :: String -> String -> (Integer, Integer)
        process [x] [y] = (read [x], read [y])
        process _ _ = (0, 0)

findFractions :: [(Integer, Integer)]
findFractions = do
  b <- [10..99]
  a <- [10..b-1]
  guard (intersect (show a) (show b) /= "0")
  let (x, y) = simplify' a b
  guard (simplify a b == simplify x y)
  return (a, b)

main :: IO ()
main = let (x, y) = foldl1 fracProd findFractions in print $ simplify x y
  where fracProd (x, y) (x', y') = (x * x', y * y')
