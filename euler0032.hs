-- Problem 32: Pandigital Products
--
-- We shall say that an n-digit number is pandigital if it makes use of all the
-- digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1
-- through 5 pandigital.
--
-- The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
-- multiplicand, multiplier, and product is 1 through 9 pandigital.
--
-- Find the sum of all products whose multiplicand/multiplier/product identity
-- can be written as a 1 through 9 pandigital.
--
-- HINT: Some products can be obtained in more than one way so be sure to only
-- include it once in your sum.

import Data.List
import Data.Char

digits = map digitToInt . show

-- Solution 1: Brute force
solution = sum . nub $ list
  where
    list = [p | x <- [1..50], y <- [100..2000], let p = x*y, pandigital x y p]
    pandigital x y p = [1..9] == sort (digits p ++ digits x ++ digits y)

main = do
    putStr "Solution: "
    putStrLn $ show solution
