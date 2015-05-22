module Euler.Date
( dayOrdinal
, nextDayOf
, weekDeltas
) where

import System.Locale
import Data.Time.Format
import Data.Time.Calendar

type DateOrdinal = Int

-- Returns the ordinal value of a day (e.g. Monday = 1)

dayOrdinal :: Day -> DateOrdinal
dayOrdinal = read . formatTime defaultTimeLocale "%u"


-- The following takes in a day, and finds the next occurring
-- day of the week (or itself if the ordinal passed aligns with
-- the given day).

nextDayOf :: DateOrdinal -> Day -> Day
nextDayOf n d
    | n < 1 || n > 7    = d
    | dayOrdinal d == n = d
    | otherwise         = nextDayOf n (addDays 1 d)


-- Returns every subsequent week from a given day
-- Note this could be placed under Sequences.hs but here it
-- felt more appropriate

weekDeltas :: Day -> [Day]
weekDeltas d = d : weekDeltas (addDays 7 d)
