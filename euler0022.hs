-- Problem 22: Names Scores
--
-- Using euler0022.txt, a 46K text file
-- containing over five-thousand first names, begin by sorting it into
-- alphabetical order. Then working out the alphabetical value for each name,
-- multiply this value by its alphabetical position in the list to obtain a name
-- score.
--
-- For example, when the list is sorted into alphabetical order, COLIN, which is
-- worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
-- would obtain a score of 938 × 53 = 49714.
--
-- What is the total of all the name scores in the file?

import Data.List
import Data.List.Split

nameValue :: [Char] -> Int
nameValue = sum . map (\c -> fromEnum c - fromEnum 'A' + 1)

scores = zipWith (*) [1..] . map nameValue

main = do
    f <- readFile "euler0022.txt"
    let names = read $ "[" ++ f ++ "]"
    putStr "Solution: "
    putStrLn $ show . sum . scores . sort $ names
