-- Problem 36: Double-base Palindromes
--
-- The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.
--
-- Find the sum of all numbers, less than one million, which are palindromic in
-- base 10 and base 2.
--
-- (Please note that the palindromic number, in either base, may not include
-- leading zeros.)

import Numeric
import Data.Char

palindrome xs = xs == reverse xs

-- Can't be even, because binary representation would end in zero so cannot be a
-- palindrome.
solution = sum [x | x <- [1,3..999999], palindrome (show x),
                                        palindrome (showIntAtBase 2 intToDigit x "")]

main = do
    putStr "Solution: "
    putStrLn $ show solution
