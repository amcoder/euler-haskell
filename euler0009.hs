-- Problem 9: Special Pythagorean Triplet
--
-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for
-- which,
--
-- a^2 + b^2 = c^2
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
--
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

-- First attempt too slow
-- isTriplet a b c = a^2 + b^2 == c^2
-- triplets = [(a,b,c) | c <- [3..], b <- [2..c], a <- [1..b], isTriplet a b c]

-- Faster version
-- Euclid's Formula:
-- https://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple
-- NOTE: Does not generate all triplets!
triplets = [(a,b,c) | m <- [2..], n <- [1..m-1],
                       let a = m^2 - n^2,
                       let b = 2*m*n,
                       let c = m^2 + n^2]


solution = head [a*b*c | (a,b,c) <- triplets, a+b+c == 1000]

main = do
    putStr "Solution: "
    putStrLn $ show solution
