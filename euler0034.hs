-- Problem 34: Digit Factorials
--
-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
--
-- Find the sum of all numbers which are equal to the sum of the factorial of
-- their digits.
--
-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.

import Data.Char

digits = map digitToInt . show

solution = sum [x | x <- [10..100000], curious x]
  where
    curious x = (==x) . sum . map (factorials!!) . digits $ x
    factorials = 1:scanl1 (*) [1..]

main = do
    putStr "Solution: "
    putStrLn $ show solution
