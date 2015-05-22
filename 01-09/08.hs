-- The four adjacent digits in the 1000-digit number that have the greatest product are 9 × 9 × 8 × 9 = 5832.
--
-- ...
--
-- Find the thirteen adjacent digits in the 1000-digit number that have the greatest product. What is the value of this product?

import Data.Char (digitToInt)
import Data.List (intercalate)
import Euler.List (subconsec)

main = do
    contents <- readFile "01-09/08.txt"
    let sProduct = product . map digitToInt
    let number = intercalate "" (words contents)
    print . maximum $ map sProduct (subconsec 13 number)
