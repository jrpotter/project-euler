-- In the 20×20 grid below, four numbers along a diagonal line have been marked in red.
--
-- ...
--
-- The product of these numbers is 26 × 63 × 78 × 14 = 1788696.
--
-- What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?

import Data.List (transpose)
import Euler.List (diagl, diagr, subconsec)

main = do
    values <- readFile "10-19/11.txt"
    let grid = map (\x -> map read (words x)) (lines values)
    let gridProd = maximum . map product . concat . map (subconsec 4)
    print . maximum $ map (\f -> (gridProd . f) grid) [id, transpose, diagl, diagr]
