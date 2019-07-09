import Test.QuickCheck

-- 4. Although there already exists a predefined function (a function, and test them using) returnin the maximum of two values, in this exercise you will define your own version.

-- a) As it is not allowed to define a function whose name coincides with a predefine done, define a function such that:

max2 :: Ord a => a -> a -> a
max2 x y
    | x > y     = x
    | otherwise = y

--  b) Define the following properties that function should fulfill and test them using QuickCheck (recall to import at the beginning of your program):
--  i.: p1_max2 maximum of x and y is either x or either y.
--  ii.: p2_max2 maximum of x and y is greater than or equal to x and greater or equal than y.
--  iii.: p3_max2 if x is greater than or equal to y, then maximum of x and y is x.
--  iv.: p4_max2 if y is greater than or equal to x, then maximum of x and y is y.

p1_max2 x y = max2 x y == x || max2 x y == y
p2_max2 x y = max2 x y >= x && max2 x y >= y
p3_max2 x y | x >= y  = max2 x y == x
p4_max2 x y | y >= x = max2 x y == y

-- All tests passed
