-- The decimal number, 585 = 1001001001_2 (binary), is palindromic in both
-- bases.
--
-- Find the sum of all numbers, less than one million, which are palindromic in
-- base 10 and base 2. 
--
-- (Please note that the palindromic number, in either base, may not include
-- leading zeros.)

backwardsBin :: Int -> String
backwardsBin x
  | x == 0  = []
  | x `mod` 2 == 0 = '0' : backwardsBin (x `div` 2)
  | otherwise = '1' : backwardsBin (x `div` 2)

isPalindrome :: String -> Bool
isPalindrome x = x == reverse x

main = print . fst $ foldl1 (\x y -> (fst x + fst y, "")) $ do
  let ls = zip [1..999999] (map backwardsBin [1..999999])
  filter (\(x, y) -> isPalindrome (show x) && isPalindrome y) ls

