-- The following iterative sequence is defined for the set of positive integers:
--
-- n →  n/2 (n is even)
-- n →  3n + 1 (n is odd)
--
-- Using the rule above and starting with 13, we generate the following sequence:
-- 13 →  40 →  20 →  10 →  5 →  16 →  8 →  4 →  2 →  1
-- It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. 
-- Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
--
-- Which starting number, under one million, produces the longest chain?
--
-- NOTE: Once the chain starts the terms are allowed to go above one million.

import Data.List (foldl')
import Euler.Sequence (collatz)
import qualified Data.PQueue.Min as PQ

type Pair = (Integer, Integer)
type Queue = PQ.MinQueue Integer

-- The following determines if the collatz sequence at the passed value is longer
-- than any before seen ones, ignoring values that we know have lesser values (namely
-- numbers that appear in the middle of a collatz sequence).
--
-- Note we never look at (best - 1) / 3 since, when going in ascending order, this
-- value's sequence length has already been computed and so is already handled in
-- the following algorithm.

longSeq :: Integer -> Pair -> Queue -> (Queue, Pair)
longSeq n bc@(best, count) q
    | n == best * 2  = (q, (n, count + 1))       -- This is the preceding value of the leading sequence
    | Just n == min' = (PQ.deleteMin q, bc)      -- We ignore these values (they've been seen before)
    | cnt' > count   = (ignore n c q, (n, cnt')) -- This number has a longer sequence than the best so far
    | otherwise      = (q, bc)                   -- This number has a shorter sequence than the best so far
    where c = collatz n
          min' = PQ.getMin q
          cnt' = (fromIntegral . length) c
          ignore x xs q' = foldl (flip PQ.insert) q' (filter (> x) xs)

-- The following is a wrapper of the above function, allowing for the use of a fold
-- to calculate the number generating the longest sequence.

main = print . snd $ foldl' (\qp n -> collatz' n qp) (PQ.empty, (0, 0)) [1..999999]
    where collatz' n qp@(q, bc@(best, count)) = max' (longSeq n bc q) qp
          max' ab@(q1, (a, b)) cd@(q2, (c, d)) = if d > b then cd else ab
