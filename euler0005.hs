-- Problem 5 - Smallest Multiple
--
-- 2520 is the smallest number that can be divided by each of the numbers from 1
-- to 10 without any remainder.
--
-- What is the smallest positive number that is evenly divisible by all of the
-- numbers from 1 to 20?

-- Euclid's Algorithm:
-- https://en.wikipedia.org/wiki/Greatest_common_divisor#Using_Euclid.27s_algorithm
largestDivisor x 0 = x
largestDivisor x y = largestDivisor y (mod x y)

-- Reduction by GCD:
-- https://en.wikipedia.org/wiki/Least_common_multiple#Reduction_by_the_greatest_common_divisor
smallestMultiple 0 0 = 0
smallestMultiple x y = (abs x) * (div (abs y) (largestDivisor x y))

solution = foldr1 smallestMultiple [1..20]

main = do
    putStr "Solution: "
    putStrLn $ show solution
