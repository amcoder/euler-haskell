-- Problem 19: Counting Sundays
--
-- You are given the following information, but you may prefer to do some
-- research for yourself.
--
-- * 1 Jan 1900 was a Monday.
-- * Thirty days has September, April, June and November. All the rest have
--   thirty-one, Saving February alone, Which has twenty-eight, rain or shine.
--   And on leap years, twenty-nine.
-- * A leap year occurs on any year evenly divisible by 4, but not on a century
--   unless it is divisible by 400.
--
-- How many Sundays fell on the first of the month during the twentieth century
-- (1 Jan 1901 to 31 Dec 2000)?

-- Solution 1: Zip the dates with the days of the week, then filter out the
-- first of the month where it is a sunday.
-- This is pretty ugly...
solution = length . filter check . zip dates $ daysOfWeek
  where
    check ((_,_,d),w) = d == 0 && w == 6
    dates = [(y,m,d) | y <- [1901..2000], m <- [0..11], d <- [0..monthLength y m - 1]]
    daysOfWeek = drop 365 $ cycle [0..6]
    monthLengths = [31,28,31,30,31,30,31,31,30,31,30,31]
    monthLength y m | mod y 4 == 0 &&
                      m == 1          = 29
                    | otherwise       = monthLengths !! m

main = do
    putStr "Solution: "
    putStrLn $ show solution
