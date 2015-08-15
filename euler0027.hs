-- Problem 27: Quadratic Primes
--
-- Euler discovered the remarkable quadratic formula:
--
-- n² + n + 41
--
-- It turns out that the formula will produce 40 primes for the consecutive
-- values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is
-- divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly
-- divisible by 41.
--
-- The incredible formula  n² − 79n + 1601 was discovered, which produces 80
-- primes for the consecutive values n = 0 to 79. The product of the
-- coefficients, −79 and 1601, is −126479.
--
-- Considering quadratics of the form:
--
-- n² + an + b, where |a| < 1000 and |b| < 1000
--
-- where |n| is the modulus/absolute value of n
-- e.g. |11| = 11 and |−4| = 4
-- Find the product of the coefficients, a and b, for the quadratic expression
-- that produces the maximum number of primes for consecutive values of n,
-- starting with n = 0.

import Data.List
import Data.Function

-- Primes from problem 3
primes = 2:filter isPrime [3,5..]
isPrime n | n <= 0    = False
          | otherwise = null . tail . primeFactors $ n
primeFactors n = factor n primes
    where
        factor n (p:ps)
            | p * p > n    = [n]
            | mod n p == 0 = p:factor (div n p) (p:ps)
            | otherwise    = factor n ps

genPrimes a b = takeWhile isPrime . map (f a b) $ [0..]
  where
    f a b n = n * (n + a) + b

-- b must be prime, because for n=0, n² + an + b == b
-- For n=1, n² + an + b == 1 + a + b == prime(p), or a = p - b - 1. So we can
--    use all primes < 1000+b+1 to get the values for a.
-- We're looking for one with a sequence longer than Euler's, so f(40) must be
--    prime.
-- This gives us a set where we know for sure that each (a,b) pair gives us
--    primes for n=0, 1, and 40, then we brute force the rest.
pairs = [((a,b), genPrimes a b) | b <- takeWhile (<1000) primes,
                                  f1 <- takeWhile (<1000+b+1) primes,
                                  let a = f1 - b - 1,
                                  isPrime (40 * (40 + a) + b)]

solution = (\((a,b),p) -> a*b) . maximumBy (compare `on` (length . snd)) $ pairs

main = do
    putStr "Solution: "
    putStrLn $ show solution
