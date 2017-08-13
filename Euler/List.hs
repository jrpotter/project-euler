module Euler.List
( subconsec
, takeSeq
, diagl
, diagr
, remove
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

-- Takes in a list and breaks it up into sublists of length 1, 2, ... until no
-- more elements remain in the list. The final list may have any number of elements
-- up until the number of lists prior plus 1.
takeSeq :: Integer -> [a] -> [[a]]
takeSeq _ [] = []
takeSeq n xs = front : takeSeq (n+1) back
    where (front, back) = splitAt (fromIntegral n) xs

-- Takes diagonals from NW to SE direction, and returns a list of lists corresponding
-- to said diagonals. If lists are not of equal size, can instead look at this function
-- as if all lists are right aligned and then diagonalized.
diagl :: [[a]] -> [[a]]
diagl [] = []
diagl (x:xs) = diagl' [] (x:xs)
    where diagl' [] []     = []
          diagl' li []     = (map last li) : diagl' (inits li) []
          diagl' [] (y:ys) = [(last y)] : diagl' [(init y)] ys
          inits = filter (not . null) . map init

-- Takes diagonals from NE to SW direction, and returns a list of lists corresponding
-- to said diagonals. Note this works similarly to diagl, but from the opposite direction.
-- When understanding this function, consider all lists to be left aligned and then diagonalized.
diagr :: [[a]] -> [[a]]
diagr = diagl . map reverse

-- Removes the element at the given index from the list.
remove :: Integer -> [a] -> (Maybe a, [a])
remove n xs = case splitAt (fromIntegral n) xs of
    (f, [])     -> (Nothing, f)
    (f, (s:ss)) -> (Just s, f ++ ss)

