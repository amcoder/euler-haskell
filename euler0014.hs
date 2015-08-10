-- Problem 14: Longest Collatz Sequence
--
-- The following iterative sequence is defined for the set of positive integers:
--
-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)
--
-- Using the rule above and starting with 13, we generate the following
-- sequence:
--
-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
--
-- It can be seen that this sequence (starting at 13 and finishing at 1)
-- contains 10 terms. Although it has not been proved yet (Collatz Problem), it
-- is thought that all starting numbers finish at 1.
--
-- Which starting number, under one million, produces the longest chain?
--
-- NOTE: Once the chain starts the terms are allowed to go above one million.

import Data.Array

-- Collatz sequence for number 'n'
collatz 1 = [1]
collatz n = n:(collatz next)
  where
    next
      |even n = div n 2
      | odd n = 3*n + 1

-- This was very slow
solution' = fst . foldr1 (\x a -> if snd x > snd a then x else a) .
                  map (\x -> (x, length . collatz $ x)) $ [1..999999]

-- Collatz count for number 'n'
collatzCount 1 = 1
collatzCount n = 1 + collatzCount next
  where
    next
      |even n = quot n 2
      | odd n = 3*n + 1

-- A little faster, but still slow.
solution'' = fst . foldr1 (\x a -> if snd x > snd a then x else a) .
                  map (\x -> (x,collatzCount x)) $ [1..999999]

-- Array of collatz counts for all numbers below 'n'
collatzCounts n = array
    where
        array = listArray (1, n) (1:[1 + count x | x <- [2..n]])
        count x
          |next x < n = array ! (next x)
          |otherwise  = 1 + count (next x)
        next n
          |even n = quot n 2
          | odd n = 3*n + 1

-- Using an array helped speed this up a lot
solution = fst . foldr f (1,1) . assocs $ collatzCounts 999999
  where
    f x a
      | snd x > snd a = x
      | otherwise     = a

main = do
    putStr "Solution: "
    putStrLn $ show solution
