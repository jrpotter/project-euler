-- You are given the following information, but you may prefer to do some research for yourself.
--
-- 1 Jan 1900 was a Monday.
-- Thirty days has September,
-- April, June and November.
-- All the rest have thirty-one,
-- Saving February alone,
-- Which has twenty-eight, rain or shine.
-- And on leap years, twenty-nine.
--
-- A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
--
-- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

import Euler.Date
import Data.Time.Calendar

-- Takes in a list of days and returns all days that align with
-- the passed position of the month
nthOfMonth :: Int -> [Day] -> [Day]
nthOfMonth n days = filter (\d -> let (_, _, d') = toGregorian d in n == d') days

main = do
    let lastDay = fromGregorian 2000 12 31
    let firstDay = nextDayOf 7 (fromGregorian 1901 1 1)
    let sundays = takeWhile (\d -> diffDays lastDay d >= 0) (weekDeltas firstDay)
    print $ length (nthOfMonth 1 sundays)

