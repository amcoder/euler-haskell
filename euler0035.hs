-- Problem 35: Circular Primes
-- The number, 197, is called a circular prime because all rotations of the
-- digits: 197, 971, and 719, are themselves prime.
--
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71,
-- 73, 79, and 97.
--
-- How many circular primes are there below one million?

import Data.Char

digits :: Int -> [Int]
digits = map digitToInt . show

undigits :: [Int] -> Int
undigits = foldl1 (\a x -> a*10+x)

primes = 2:filter isPrime [3,5..]
isPrime = null . tail . primeFactors
primeFactors n = factor n primes
    where
        factor n (p:ps)
            | p * p > n    = [n]
            | mod n p == 0 = p:factor (div n p) (p:ps)
            | otherwise    = factor n ps

rotate n xs = take (length xs) . drop n $ (cycle xs)
rotations xs = take (length xs) . iterate (rotate 1) $ xs

solution = length $ 2:5:[p | p <- primes', let dp = digits p,
                                           all odd dp, all (/=5) dp,
                                           rotationsPrime dp]
  where
    primes' = takeWhile (<1000000) primes
    rotationsPrime xs = all isPrime . map undigits . rotations $ xs

main = do
    putStr "Solution: "
    putStrLn $ show solution
