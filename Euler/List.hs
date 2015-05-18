module Euler.List
( subconsec
, diagl
, diagr
) where

-- Takes in a list and returns all sublists of consecutive elements of length n
-- All other lists (such as those at the nth index from the end) are ignored
--
-- In order to avoid checking the length, or filtering through the list once
-- completed for sublists of size n, we instead keep two separate lists, filling
-- one up to n elements and then cycling through the remainder of the second
-- until empty.

subconsec :: Integer -> [a] -> [[a]]
subconsec _ [] = []
subconsec n a@(x:xs) = let t = take' n [] a in 
    case t of [] -> []
              _  -> t : subconsec n xs
    where take' 0 r _ = r
          take' _ _ [] = []
          take' n r (x:xs) = take' (n-1) (r ++ [x]) xs


-- Goes through a list of lists, returning the init of each list (or nothing at all
-- if the list is empty). A wrapper function for use elsewhere (e.g. diagl)

inits :: [[a]] -> [[a]]
inits = filter (not . null) . map init


-- Takes diagonals from NW to SE direction, and returns a list of lists corresponding
-- to said diagonals. If lists are not of equal size, can instead look at this function
-- as if all lists are right aligned and then diagonalized.

diagl :: [[a]] -> [[a]]
diagl [] = []
diagl (x:xs) = diagl' [] (x:xs)
    where diagl' [] []     = []
          diagl' li []     = (map last li) : diagl' (inits li) []
          diagl' [] (y:ys) = [(last y)] : diagl' [(init y)] ys
          diagl' li (y:ys) = let li' = li ++ [y] in (map last li') : diagl' (inits li') ys  


-- Takes diagonals from NE to SW direction, and returns a list of lists corresponding
-- to said diagonals. Note this works similarly to diagl, but from the opposite direction.
-- When understanding this function, consider all lists to be left aligned and then diagonalized.

diagr :: [[a]] -> [[a]]
diagr = diagl . map reverse
