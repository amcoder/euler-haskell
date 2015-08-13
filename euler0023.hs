-- Problem 23: Non-abundant Sums
--
-- A perfect number is a number for which the sum of its proper divisors is
-- exactly equal to the number. For example, the sum of the proper divisors of
-- 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
--
-- A number n is called deficient if the sum of its proper divisors is less than
-- n and it is called abundant if this sum exceeds n.
--
-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
-- number that can be written as the sum of two abundant numbers is 24. By
-- mathematical analysis, it can be shown that all integers greater than 28123
-- can be written as the sum of two abundant numbers. However, this upper limit
-- cannot be reduced any further by analysis even though it is known that the
-- greatest number that cannot be expressed as the sum of two abundant numbers
-- is less than this limit.
--
-- Find the sum of all the positive integers which cannot be written as the sum
-- of two abundant numbers.

import Data.List
import Data.IntSet (toList, fromList)
import Data.Array

-- Primes from problem 3
primes = 2:filter isPrime [3,5..]
isPrime = null . tail . primeFactors
primeFactors n = factor n primes
    where
        factor n (p:ps)
            | p * p > n    = [n]
            | mod n p == 0 = p:factor (div n p) (p:ps)
            | otherwise    = factor n ps

-- Number of factors of 'n'
-- https://en.wikipedia.org/wiki/Divisor_function
--
-- Calculate prime factors
-- Product of each exponent + 1
-- 120 = 2*2*2*3*5 =  2^3  *  3^1  *  5^1
-- count =           (3+1) * (1+1) * (1+1) = 16
divisor = product . map ((1+) . length) . group . primeFactors
-- sum =             (1+2^1+2^2+2^3) * (1+3^1) * (1+5^1)
sigma = product . map ((+1) . foldl1 (\a x -> x+a*x)) . group . primeFactors
aliquot n = sigma n - n

maxN = 28123

isAbundants = listArray (1,maxN) $ map (\n -> aliquot n > n) [1..maxN]
isAbundant x = isAbundants ! x
abundantNumbers = filter isAbundant [1..maxN]

solution = sum [x | x <- [1..maxN], f x]
  where
    f x = null [() | a <- ns x, isAbundant (x-a)]
    ns x = takeWhile (\a -> a <= div x 2) abundantNumbers

main = do
    putStr "Solution: "
    putStrLn $ show solution
