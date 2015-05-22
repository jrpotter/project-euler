-- If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
--
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
--
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 
-- (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

import Data.Char (isLetter)
import qualified Data.Map as Map

-- Series of mappings between numbers and characters

ones :: Map.Map Int String
ones = Map.fromList [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five"), (6, "six"), (7, "seven"), (8, "eight"), (9, "nine")]

teens :: Map.Map Int String
teens = Map.fromList [(10, "ten"), (11, "eleven"), (12, "twelve"), (13, "thirteen"), (14, "fourteen")]

prefixes :: Map.Map Int String
prefixes = Map.union (Map.fromList [(3, "thir"), (4, "for"), (5, "fif"), (8, "eigh")]) ones

tens :: Map.Map Int String
tens = Map.union (Map.fromList [(20, "twen")]) (Map.mapKeys (*10) prefixes)


-- Takes in an integer and converts it into its corresponding "word" value
-- as according to the problem description.

convert :: Int -> String
convert n
    | n < 10                       = Map.findWithDefault "" n ones
    | n < 15                       = Map.findWithDefault "" n teens
    | n < 20                       = Map.findWithDefault "" (n `mod` 10) prefixes ++ "teen"
    | n < 100 && n `mod` 10 == 0   = Map.findWithDefault "" n tens ++ "ty"
    | n < 100                      = convert (10 * (n `div` 10)) ++ "-" ++ convert (n `mod` 10)
    | n < 1000 && n `mod` 100 == 0 = convert (n `div` 100) ++ " hundred"
    | n < 1000                     = convert (100 * (n `div` 100)) ++ " and " ++ convert (n `mod` 100)
    | n == 1000                    = "one thousand"
    | otherwise                    = ""


-- Reads in only letters, skipping over spaces

countLetters :: String -> Int
countLetters "" = 0
countLetters (x:xs) = countLetters xs + if isLetter x then 1 else 0


-- Main Function

main = print . sum $ map (countLetters . convert) [1..1000]
