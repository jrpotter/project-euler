-- Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, 
-- there are exactly 6 routes to the bottom right corner.
--
-- How many such routes are there through a 20×20 grid?

-- This is less a programming problem as a simple math problem. If we let R represent a right motion,
-- and D a downward one, a possible path through the grid could be represented by:
--
-- RRRRRRRRRRRRRRRRRRRRDDDDDDDDDDDDDDDDDDDD.
--
-- So, given 40 spaces, how many ways can I choose 20 of them to be R? This is simply (40 nCr 20).

import Euler.Algebra (factorial, factorial')

main = print $ (factorial' 40 20) `div` (factorial 20)
