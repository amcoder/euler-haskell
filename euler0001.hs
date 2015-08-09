-- Problem 1 - Multiple of 3 and 5
--
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we
-- get 3, 5, 6 and 9. The sum of these multiples is 23.
--
-- Find the sum of all the multiples of 3 or 5 below 1000.

multiplesOf3Or5 = [x | x <- [1..], multipleOf3Or5 x]

multipleOf3Or5 x = mod x 3 == 0 || mod x 5 == 0

solution = sum . takeWhile (< 1000) $ multiplesOf3Or5

main = do
    putStr "Solution: "
    putStrLn $ show solution
