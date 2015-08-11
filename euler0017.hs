-- Project 17: Number Letter Counts
--
-- If the numbers 1 to 5 are written out in words: one, two, three, four, five,
-- then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
--
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out
-- in words, how many letters would be used?
--
--
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
-- forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
-- letters. The use of "and" when writing out numbers is in compliance with
-- British usage.

import Data.Char

-- Solution 1: I can figure this out by hand using common patterns.
--
-- 1-9: one two three four five six seven eight nine = 36
-- 10-19: ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen = 70
-- 20-90: twenty thirty forty fifty sixty seventy eighty ninety = 46
-- "and" = 3
-- "hundred = 7
-- "one thousand" = 11
--
-- thousand = "one thousand"          = 11
-- hundreds = (36 * 100) + (7 * 900)  = 9900
-- and      = 3 * 990                 = 2700
-- teens    = (36 + 70) * 10          = 1060
-- tens     = (46 * 100) + (36 * 100) = 8200

count =
  -- "one thousand": Only one of them
  11 +
  -- hundreds place: Each one is repeated 100 times
  (36 * 100) +
  -- "hundred": There are 900 of them
  (7 * 900) +
  -- "and": 99 per hundred
  (3 * 99 * 9) +
  -- 1-9: 10 of them
  (36 * 10) +
  -- 10-19: 10 of them
  (70 * 10) +
  -- 20-99: 10 of them
  (((36 * 8) + (46 * 10)) * 10)

-- Solution 2: Brute force, for fun

ones = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
          "sixteen", "seventeen", "eighteen", "nineteen", "twenty"]
tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
          "ninety"]

toEnglish n | n >= 1000 = (ones !! ((div n 1000) - 1)) ++ " thousand" ++ andIf (mod n 1000)
            | n >= 100  = (ones !! ((div n 100) - 1)) ++ " hundred" ++ andIf (mod n 100)
            | n >= 20   = (tens !! ((div n 10) - 2)) ++ dashIf (mod n 10)
            | n >= 10   = (teens !! (n - 10))
            | n > 0     = (ones !! (n - 1))
            | otherwise = ""
  where
    andIf n | n > 100   = spaceIf n
            | n > 0     = " and" ++ spaceIf n
            | otherwise = ""
    spaceIf 0 = ""
    spaceIf n = " " ++ toEnglish n
    dashIf 0 = ""
    dashIf n = "-" ++ toEnglish n

solution = sum . map (length . filter isAlpha . toEnglish) $ [1..1000]

main = do
    putStr "Solution: "
    putStrLn $ show solution
