-- Problem 28: Number Spiral diagonals
--
-- Starting with the number 1 and moving to the right in a clockwise direction a
-- 5 by 5 spiral is formed as follows:
--
-- 21 22 23 24 25
-- 20  7  8  9 10
-- 19  6  1  2 11
-- 18  5  4  3 12
-- 17 16 15 14 13
--
-- It can be verified that the sum of the numbers on the diagonals is 101.
--
-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral
-- formed in the same way?

-- Solution 1: Iterative solution
-- Generate list of all corners, then sum them.
solution = sum . scanl (+) 1 . concat . map (replicate 4) $ [2,4..1000]

-- Solution 2: Polynomial per ring, iterative
-- http://www.mathblog.dk/project-euler-28-sum-diagonals-spiral/
-- Top right corner of ring n = area of square = side^2 = (2n+1)^2
-- Top left corner is previous corner minus the side length = (2n+1)^2 - 2n
-- ...and so on for other corners
-- f(n) = 4(2n+1)^2 - 12n
solution' = (+1) . sum . map (\n -> 4*(2*n+1)^2 - 12*n) $ [1..500]

-- Solution 3: Closed form polynomial
-- http://www.mathblog.dk/project-euler-28-sum-diagonals-spiral/
diagSum n = truncate $ 16/3*n^3 + 10*n^2 + 26/3*n + 1
solution'' = diagSum 500

main = do
    putStr "Solution: "
    putStrLn $ show solution
