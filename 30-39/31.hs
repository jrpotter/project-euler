-- In England the currency is made up of pound, P, and pence, p, and there are
-- eight coins in general circulation:
--
-- 1p, 2p, 5p, 10p, 20p, 50p, 1P, and 2P.
--
-- It is possible to make 2P in the following way:
--
-- 1 x 1P + 1 x 50p + 2 x 20p + 1 x 5p + 1 x 2p + 3 x 1p.
--
-- How many different ways can 2P be made using any number of coins?

import qualified Data.Strict.Map as M

coins :: [Int]
coins = [1, 2, 5, 10, 20, 50, 100, 200]
