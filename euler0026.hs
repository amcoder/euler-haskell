-- Problem 26: Reciprocal Cycles
--
-- A unit fraction contains 1 in the numerator. The decimal representation of
-- the unit fractions with denominators 2 to 10 are given:
--
-- 1/2  =   0.5
-- 1/3  =   0.(3)
-- 1/4  =   0.25
-- 1/5  =   0.2
-- 1/6  =   0.1(6)
-- 1/7  =   0.(142857)
-- 1/8  =   0.125
-- 1/9  =   0.(1)
-- 1/10 =   0.1
-- Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
-- seen that 1/7 has a 6-digit recurring cycle.
--
-- Find the value of d < 1000 for which 1/d contains the longest recurring cycle
-- in its decimal fraction part.

import Data.List
import Data.Function

-- Primes from problem 3
primes = 2:filter isPrime [3,5..]
isPrime = null . tail . primeFactors
primeFactors n = factor n primes
    where
        factor n (p:ps)
            | p * p > n    = [n]
            | mod n p == 0 = p:factor (div n p) (p:ps)
            | otherwise    = factor n ps

-- https://en.wikipedia.org/wiki/Multiplicative_order
order a n = order' a 1 n
  where
    order' a k n | mod (a^k) n == 0 = 0
                 | mod (a^k) n == 1 = k
                 | otherwise        = order' a (k+1) n

-- https://en.wikipedia.org/wiki/Repeating_decimal
-- The repetition length is the LCM of the multiplicative order of the prime
-- factors(not 2 or 5) of a number.
reciprocalRepetitionLength n = lcm' . map length' . primes' $ n
  where
    lcm' [] = 0
    lcm' l = foldl1 lcm l
    primes' = map (\g -> (head g, length g)) . group . filter (\p -> p/=2 && p/=5) . primeFactors
    length' (p,k) | mod (10^(p-1)-1) (p^k) == 0 = order 10 p
                  | otherwise                   = p^(k-1) * (order 10 p)

-- The longest repetition possible is d-1, so we can just search backwards for
-- the first one that equals d-1.
solution = head . dropWhile (\x -> reciprocalRepetitionLength x < x - 1) $ [999,998..1]
  where
    length' x = (x,reciprocalRepetitionLength x)

main = do
    putStr "Solution: "
    putStrLn $ show solution
