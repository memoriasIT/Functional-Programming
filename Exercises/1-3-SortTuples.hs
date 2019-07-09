import Test.QuickCheck

-- 3. This exercise is on sorting tuples.
-- a) Define an overloaded function for ordered types taking a two components tuple (whose elements have the same type) and returning the corresponding ascending sorted tuple:

sort2 :: Ord a => (a, a) -> (a, a)
sort2 (x, y)
    | x <= y      = (x, y)
    | y < x       = (y, x)


-- b) Copy the following properties for function

p1_sort2 x y = sorted (sort2 (x,y))
    where sorted (x,y) = x <= y

p2_sort2 x y = sameElements (x, y) (sort2 (x,y))
    where
        sameElements (x, y) (x', y') = (x == x' && y == y') || (x == y' && y == x')

-- Understand each of these properties and test them using QuickCheck.
-- quickCheck p1_sort2
-- +++ OK, passed 100 tests.
 
-- quickCheck p2_sort2
-- +++ OK, passed 100 tests.


-- c) Define an overloaded function taking a tuple with three components of the same type and returning it sorted in ascending order:


-- We can implement this with quicksort (this would sort n elements not only 3)
quicksort:: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallsort = quicksort [a | a <- xs, a <= x]
        bigsort = quicksort [a | a <- xs, a > x]
    in smallsort ++ [x] ++ bigsort


-- But how I think we are supposed to do it according to b) (Unrolled Bubble sort)
-- Using guards

sort3 :: Ord a => (a, a, a) -> (a, a, a)
sort3 (x, y, z)
    | x > y     = sort3 (y, x, z)
    | y > z     = sort3 (x, z, y)
    | x > z     = sort3 (z, y, x)
    | otherwise = (x, y, z)


-- d) Write similar properties to the ones in paragraph b) but for QuickCheck.

p1_sort3 x y z = sorted (sort3 (x,y,z))
    where sorted (x,y,z) = x <= y && y <= z

p2_sort3 x y z = sameElements (x, y, z) (sort3 (x,y,z))
    where
        sameElements (x,y, z) (x', y', z') = (x == x' && y == y' && z == z') || (x == y' && y == x' && x == z')

-- All tests passed for both
