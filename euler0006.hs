-- Problem 6 - Sum Square Difference
--
-- The sum of the squares of the first ten natural numbers is,
--
-- 1^2 + 2^2 + ... + 10^2 = 385
--
-- The square of the sum of the first ten natural numbers is,
--
-- (1 + 2 + ... + 10)^2 = 55^2 = 3025
--
-- Hence the difference between the sum of the squares of the first ten natural
-- numbers and the square of the sum is 3025 − 385 = 2640.
--
-- Find the difference between the sum of the squares of the first one hundred
-- natural numbers and the square of the sum.

sumOfSquares = sum . map (^2)

squareOfSum = (^2) . sum

difference xs = squareOfSum xs - sumOfSquares xs

solution = difference [1..100]

main = do
    putStr "Solution: "
    putStrLn $ show solution
