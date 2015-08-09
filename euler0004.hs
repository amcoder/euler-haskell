-- Problem 4 - Largest Palindrome Product
--
-- A palindromic number reads the same both ways. The largest palindrome made
-- from the product of two 2-digit numbers is 9009 = 91 × 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.

threeDigitProducts = [x * y | x <- [100..999], y <- [x..999]]

isPalindrome xs = reverse xs == xs

largestPalindrome = maximum . filter (isPalindrome . show)

solution = largestPalindrome threeDigitProducts

main = do
    putStr "Solution: "
    putStrLn $ show solution
