-- Problem 21: Amicable Numbers
--
-- Let d(n) be defined as the sum of proper divisors of n (numbers less than n
-- which divide evenly into n).
-- If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and
-- each of a and b are called amicable numbers.
--
-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44,
-- 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4,
-- 71 and 142; so d(284) = 220.
--
-- Evaluate the sum of all the amicable numbers under 10000.

import Data.List

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

amicablePairs = [(a,b) | a <- [1..], let b = aliquot a, a < b, aliquot b == a]

solution = sum . map addTup . takeWhile under10k $ amicablePairs
  where
    addTup (a,b) = a+b
    under10k (a,b) = a < 10000

main = do
    putStr "Solution: "
    putStrLn $ show solution
