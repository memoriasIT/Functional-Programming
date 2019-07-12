
--16. This exercise studies integer division of numbers.
--a) Define function taking two numbers and testing whether its first argument exactly divides its second one. For instance:

divides :: Integer -> Integer -> Bool
divides x y = mod y x == 0


--b) Read, understand and test using QuickCheck the following property for
--function:

p1_divides x y = y/= 0 && y `divides` x ==> div x y * y == x
-- +++ OK, passed 100 tests; 904 discarded.


-- c) Define a property to test using QuickCheck that if a number exactly divides two numbers, it also exactly divides their sum.

p2_divides x y z = (x /= 0 && y/=0 && z/=0 && divides x y && divides x z) ==> divides x (y+z) == True

-- *** Gave up! Passed only 52 tests; 1000 discarded tests.
