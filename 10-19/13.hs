-- Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.
--
-- ...

main = do
    content <- readFile "10-19/13.txt"
    let numbers = map read (lines content) :: [Integer]
    print . take 10 $ show (sum numbers)

