-- Problem 15: Lattice Paths
--
-- Starting in the top left corner of a 2×2 grid, and only being able to move to
-- the right and down, there are exactly 6 routes to the bottom right corner.
--
--
-- How many such routes are there through a 20×20 grid?

-- Solution:
-- At any point, the number of paths is the sum of the number of paths to the
-- right and the number of paths to the bottom. So, starting at the bottom
-- right, there are zero paths. Any point on the bottom and right sides has one
-- path to the end.

-- 3x3 grid
--
--  20-10--4--1
--   |  |  |  |
--  10--6--3--1
--   |  |  |  |
--   4--3--2--1
--   |  |  |  |
--   1--1--1--0

-- This works but is VERY slow(like, 3 days) for a 20x20 grid though.
pathCount' w h x y
    | x == w = 1
    | y == h = 1
    | otherwise = right + down
    where
      right = pathCount' w h (x+1) y
      down = pathCount' w h x (y+1)

-- Solution 2: Same as above, but we start at the bottom right instead
-- This is MUCH faster because we're using a list to generate the calculations
-- and each calculation only uses values that have already been calculated
-- previously.
-- NOTE: This still gets really slow with large grids. (500x500)
pathCounts w h = reverse list
  where
    list = [count x y | y <- [0..h], x <- [0..w]]
    count x y
      |x == 0 && y == 0 = 0
      |x == 0 || y == 0 = 1
      |otherwise        = (left x y) + (up x y)
    left x y = list !! (index (x-1) y)
    up x y = list !! (index x (y-1))
    index x y = y * (w+1) + x

pathCount w h = head (pathCounts w h)

-- Solution 3: Basically the same concept as solution 2, but instead we generate
-- an infinite grid of counts. Since the path count is calculated using counts
-- we've previously calculated, this works really quickly, even for very large
-- grids (500x500).
-- Usage: 'grid !! x !! y'
-- Example: grid !! 20 !! 20
grid = [row y | y <- [0..]]
  where
    row 0 = repeat 1 :: [Integer]
    row y = 1:[count x y | x <- [1..]]
    count x y = (grid !! y !! (x-1)) + (grid !! (y-1) !! x)

solution = grid !! 20 !! 20

main = do
    putStr "Solution: "
    putStrLn $ show solution
