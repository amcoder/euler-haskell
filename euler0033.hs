-- Problem 33: Digit Cancelling Fractions
--
-- The fraction 49/98 is a curious fraction, as an inexperienced mathematician
-- in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which
-- is correct, is obtained by cancelling the 9s.
--
-- We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
--
-- There are exactly four non-trivial examples of this type of fraction, less
-- than one in value, and containing two digits in the numerator and
-- denominator.
--
-- If the product of these four fractions is given in its lowest common terms,
-- find the value of the denominator.

import Data.Char
import Data.Ratio
import Data.List

digits = map digitToInt . show

-- Solution: Find all fractions where 'cancelling' a common digit in both the
-- numerator and denominator is a reduced form of the fraction, but the number
-- cancelled is not a factor of both the numerator and denominator.
solution = denominator . product $ list
  where
    list = [n % d | n <- [10..99], d <- [n+1..99], x <- digits n, x > 0,
                    -- Not a common factor
                    mod n x /= 0 || mod d x /= 0,
                    let ns = digits n, let ds = digits d,
                    -- Cancelled digit is in denominator
                    elem x ds,
                    let d2 = head . delete x $ ds, let n2 = head . delete x $ ns,
                    d2 > 0,
                    n % d == n2 % d2]

main = do
    putStr "Solution: "
    putStrLn $ show solution
