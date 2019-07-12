import Test.QuickCheck


--11. Define an overloaded function named and returning over integral numbers taking to values x and y, if x is a multiple of y. For example:

-- isMultiple 9 3 => True
-- isMultiple 7 3 => False

isMultiple :: Integer -> Integer -> Bool
isMultiple x y = (x `mod` y == 0)
