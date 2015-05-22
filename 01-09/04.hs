-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
-- 
-- Find the largest palindrome made from the product of two 3-digit numbers.

isPalindrome :: Int -> Bool
isPalindrome x = let number = show x in number == reverse number
    
main = print . maximum $ filter isPalindrome $ [x*y | x <- [999, 998..100], y <- [x, x-1..100]]
