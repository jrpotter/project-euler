-- Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names,
-- begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value
-- by its alphabetical position in the list to obtain a name score.
--
-- For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th
-- name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.
--
-- What is the total of all the name scores in the file?

import Data.Char (ord)
import Data.List (sort)
import Data.List.Split (splitOn)

nameVal :: Int -> String -> Int
nameVal n name =  n * (sum $ map (\x -> ord x - 64) name)

main = do
    contents <- readFile "names.txt"
    let names = sort $ map (init . tail) (splitOn "," contents)
    print . sum $ map (\(x, y) -> nameVal x y) (zip [1..] names)
