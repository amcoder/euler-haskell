-- Problem 24: Lexicographic Permutations
--
-- A permutation is an ordered arrangement of objects. For example, 3124 is one
-- possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
-- are listed numerically or alphabetically, we call it lexicographic order. The
-- lexicographic permutations of 0, 1 and 2 are:
--
-- 012   021   102   120   201   210
--
-- What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4,
-- 5, 6, 7, 8 and 9?

import Data.List

digits = ['0'..'9']

solution = head . drop 999999 . perms $ digits

perms [] = [[]]
perms xs = concat . map perm $ xs
  where
    perm x = map (x:) (perms (delete x xs))

main = do
    putStr "Solution: "
    putStrLn $ show solution
