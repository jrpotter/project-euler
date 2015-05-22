-- By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.
--
-- 3
-- 7 4
-- 2 4 6
-- 8 5 9 3
--
-- That is, 3 + 7 + 4 + 9 = 23.
--
-- Find the maximum total from top to bottom of the triangle below:
--
-- ...
--
-- NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route. 
-- However, Problem 67, is the same challenge with a triangle containing one-hundred rows; it cannot be 
-- solved by brute force, and requires a clever method! ;o)

import Euler.List (takeSeq)

-- We work by "collapsing" the triangle upward. The bottom row gets erased, and is replaced by the second
-- to last row, where each index is now equal to the previous value plus the max of the two possible
-- routes it could have gone. We continue this until we hit the first row.
--
-- Note the following will only work properly if the second list is larger than the first. This is not
-- tested.

collapse :: [[Int]] -> [Int]
collapse [] = []
collapse (x:xs) = maxPath x (collapse xs)
    where maxPath [] _ = []
          maxPath xs [] = xs
          maxPath (x:xs) (y1:y2:ys) = x + max y1 y2 : maxPath xs (y2:ys)

main = do
    values <- readFile "10-19/18.txt"
    let nums = map read (words values) :: [Int]
    print . collapse $ takeSeq 1 nums
