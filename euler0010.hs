-- Problem 10 - Summation of Primes
--
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
-- Find the sum of all the primes below two million.

-- Primes from problem 3
primes = 2:filter isPrime [3,5..]
isPrime = null . tail . primeFactors
primeFactors n = factor n primes
    where
        factor n (p:ps)
            | p * p > n    = [n]
            | mod n p == 0 = p:factor (div n p) (p:ps)
            | otherwise    = factor n ps

-- The solution from problem 3 takes over 40 seconds to complete on my machine.
-- We will soon need to find a faster method of generating primes.

solution = sum . takeWhile (< 2000000) $ primes

main = do
    putStr "Solution: "
    putStrLn $ show solution
