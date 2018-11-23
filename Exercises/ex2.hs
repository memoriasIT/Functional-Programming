import Data.List
import Test.QuickCheck

------------------------------------------------------------------------
-- 1. [DONE] Check if elements of a list are different                --
------------------------------------------------------------------------
{-

Desired Output:
allDIfferent[1,7,3] => True              allDifferent[1,7,3,1] => False

+-------+----------------+
| NOTES | notElem        |
+-------+----------------+
Returns False if the list contains an item equal to the first argument

notElem :: (Eq a, Foldable t) => a -> t a -> Bool

notElem 5 [1,2,3] -> True
notElem 2 [1,2,3] -> False

-}

allDifferent :: (Eq a) => [a] -> Bool
allDifferent [] = True
allDifferent (x:xs) = notElem x xs && allDifferent xs



------------------------------------------------------------------------
-- 2. [DONE]  Create replicate function                               --
------------------------------------------------------------------------
{-
+-------+----------------+
| NOTES | replicate      |
+-------+----------------+

replicate ::Int -> a -> [a]

Creates list with length Int of element a

replicate 2 0 = [0,0]
replicate 4 'a' = "aaaa"

-}

replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' x a = a : replicate' (x-1) a


-- b)	Read and understand the following property on:

p_replicate' n x = n >= 0 && n <= 1000 ==>
    length(filter (==x) xs ) == n
    && length (filter (/=x) xs) == 0
        where xs = replicate' n x

-- c) Test this property using QuickCheck.

-- +++ OK, passed 100 tests; 81 discarded.


------------------------------------------------------------------------
-- 3. [DONE] Generate divisors of a natural number                    --
------------------------------------------------------------------------

{-

Desired Output:

divisors 10 => [1, 2, 5, 10]
divisors' (-10) => [-10, -5, -2, -1, 1, 2, 5, 10]

+-------+----------------+
| NOTES | rem            |
+-------+----------------+
rem is similar to modulus but works with negative numbers

rem :: Integral a => a -> a -> a

2 `rem` 4 -> 2


-}


divisors' x = divcalc 1 
    where
        divcalc n | abs x == n  = [-x,x]
        divcalc n               = if x `rem` n == 0 then (-n): n : (divcalc (n+1) ) else divcalc (n+1) 



------------------------------------------------------------------------
-- 4. [DONE] Generate greatest common divisor                         --
------------------------------------------------------------------------

{-
+-------+--------------------+
| NOTES | List comprehension |
+-------+--------------------+

[ expression | pattern <- list ]


-- Example 1:

multiply2 n = [x*2 | x <- [1..  n]]

   multiply2 3 -> [2,4,6]

-- Example 2:

multgrt10 n = [x*2 | x <- [1.. n], x*2 > 10]
   
   multgrt10 6 --> [12]


-- Example 3:

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]  

    Output:  boomBangs [1, 2, 12, 13] --> ["BOOM!","BANG!"]


-- Example 4:

length' xs = sum [ 1 | _ <- xs]
    Output: returns length of xs

-}

-- 3. A) Define greatest common divisor

gcdiv x y
    | x /= 0 && y /=  0     = maxcommon x y
    | otherwise             = error "X or Y are not different than 0"
    where
        maxcommon a 0 = a
        maxcommon a b = maxcommon b (rem a b) 


-- 3. B) Define and test with quickCheck

p_gcdiv x y z =  x > 0 && y > 0 && z > 0 ==>  gcdiv (z*x) (z*y) == z * gcdiv x y  

-- quickCheck (p_gcdiv :: Int -> Int -> Int -> Property)
-- +++ OK, passed 100 tests; 729 discarded.

-- 3. C) Define least common multiple

{-
Knowing:

gcdiv x y * lcmul x y = x * y


Desired Output:

lcmul 9 15 -> 45
lcmul 30 75 -> 150

-}

lcmul x y = x * y `div` gcdiv x y


------------------------------------------------------------------------
-- 5. [DONE] Prime numbers                                            --
------------------------------------------------------------------------

-- 5. A) Check if a number is prime

prime :: Integer -> Bool
prime x = checkprime x 2
    where
        checkprime n y
            | n == y    = True 
            | n == 2    = True
            | n == 1    = True
            | otherwise = if n `mod` y == 0 then False  else checkprime n (y+1)

-- 5. B) List Comprehension

primesUpto :: Integer -> [Integer]
primesUpto x = [ x | x <- [1..x], prime x]


-- 5. C)  Use filter


primesUpto' x = filter prime [1..x]

-- 5. D) Test with quickCheck

p1_primes x = primesUpto x == primesUpto' x

-- +++ OK, passed 100 tests.

------------------------------------------------------------------------
-- 6. [DONE] Define take and drop functions                           --
------------------------------------------------------------------------

{-

+-------+--------------------+
| NOTES | Zip                |
+-------+--------------------+
Joins two lists in a lists of tuples

zip :: [a] -> [b] -> [(a, b)]

zip [1, 2, 3] [True, True, False] => [(1, True), (2, True), (3, False)]
zip [1, 2] [True, True, False] => [(1, True), (2, True) ]

-}

-- 6. A) Define take 

{-
Desired Output:

take' 3 [0,1,2,3,4,5] => [0,1,2]
take' 0 [0,1,2,3,4,5] => []
take' 5 [0,1,2]       => [0,1,2]

-}
-- [ expression | pattern <- list ]
take' :: Int -> [a] -> [a]
take' n xs = [ x  | (p,x) <- zip [0..(n-1)]  xs]


-- 6. B) Define Drop

{-
Desired Output:

drop' 3 [0,1,2,3,4,5] => [3,4,5]
drop' 0 [0,1,2,3,4,5] => [0,1,2,3,4,5]
drop' 5 [0,1,2]       => []

-}
drop' :: Int -> [a] -> [a]
drop' n xs = [ x | (p,x) <- zip [1..] xs, p > n ]


-- 6. C) Check with quickCheck

-- for n >= 0 &&  list xs
-- take' n xs (++) drop' n xs == xs

p_takendrop n xs = n >= 0 ==> take' n xs ++  drop' n xs == xs

-- +++ OK, passed 100 tests; 115 discarded.


------------------------------------------------------------------------
-- 7. [TODO 7. B] Define Concatenation                                     --
------------------------------------------------------------------------

{-
+-------+--------------------+
| NOTES | concat             |
+-------+--------------------+

concat :: Foldable t => t [a] -> [a]

concat [[1,2,3], [5,6], [8,0,1,2]] → [1,2,3,5,6,8,0,1,2]

Result is same as:
[1,2,3] ++ ([5,6] ++ ([8,0,1,2] ++ []))


+-------+--------------------+
| NOTES | fold               |
+-------+--------------------+
fold (+) [1,2,3,4,5] → 1+2+3+4+5 = 15


foldr :: Foldable t => (a → b → b) → b → t a → b
foldl :: Foldable t => (b → a → b) → b → t a → b

Fold Left / Fold Right

foldr (+) 0 [1..1000000] -->
1 + (foldr (+) 0 [2..1000000]) -->
1 + (2 + (foldr (+) 0 [3..1000000])) -->
1 + (2 + (3 + (foldr (+) 0 [4..1000000]))) -->
1 + (2 + (3 + (4 + (foldr (+) 0 [5..1000000])))) -->


-}

-- 7. A) Concat with fold

concat' :: [[a]] -> [a]
concat' = foldl (++) []


-- 7. B) Concat with list comprehension

-- concat'' :: [[a]] -> [a]
-- concat'' x = [ xs | xs <- x] 


------------------------------------------------------------------------
-- 8. [DONE] Explain what a function does                             --
------------------------------------------------------------------------

unknown :: (Ord a) => [a] -> Bool
unknown xs = and [x <= y | (x,y) <- zip xs (tail xs) ]

{-
*Main> zip ([1,4,3]) (tail [1,4,3, 5])
[(1,4),(4,3),(3,5)]
*Main> zip ([1,4,3]) (tail [1,4,3])
[(1,4),(4,3)]

*Main> [x <= y | (x,y) <- zip [1,3,5] (tail [1,3,5]) ]
[True,True]
*Main> [x <= y | (x,y) <- zip [1,4,3] (tail [1,4,3]) ]
[True,False]
[(1,4),(4,3)]


zip xs (tail xs)
Will generate tuples of elements (n-1) with the xs and tail of xs

Tuples look like:
[(1,4),(4,3)]

X must be lower or equal than y, in this case:
[True,False]

Then all tuples are operated with the AND logic gate, giving in this case False
-}

------------------------------------------------------------------------
-- 9. [TODO 9. A]  Define insert function                                  --
------------------------------------------------------------------------

{-
+-------+--------------------+
| NOTES | takeWhile          |
+-------+--------------------+

takeWhile :: (a -> Bool) -> [a] -> [a]

takeWhile even [2,4,6,8,11,13,16,20] → [2,4,6,8]

takeWhile (<5) [2,4,6,1] → [2,4]

+-------+--------------------+
| NOTES | dropWhile          |
+-------+--------------------+

dropWhile even [2,4,6,8,11,13,16,20] → [11,13,16,20]
dropWhile (<5) [2,4,6,1] → [6,1]



//insert
Desired output:
insert 5 [1,2,4,7,8,11]  → [1,2,4,5,7,8,11]
insert 2 [1,2,4,7,8,11]  → [1,2,2,4,7,8,11]
insert 0 [1,2,4,7,8,11]  → [0,1,2,4,7,8,11]
insert 20 [1,2,4,7,8,11] → [1,2,4,7,8,11,20]
-}

-- 9. A) Define insert function

-- insert x xs  = pre ++ [x] ++ post 
--     where
--         pre  = takeWhile (<x) xs
--         post = dropWhile (<x) xs


-- 9. B) Test insert with quickCheck

--p1_insert x xs = unknown xs ==> unknown (insert x xs)

-- 9. C) Explain why we can use insert to sort

{-
This is known as insertion sort, we get a list and start inserting values
which are put in order to sor a given list.

If we consider the example:
[9,3,7]
We would operate it as:
9 `insert (3 `insert` (7 `insert` []))
9 `insert (3 `insert` [7])
9 `insert [3,7])
[3,7,9]
-}

-- 9. D) Use foldr and insert to define isort
-- Desired output:
-- isort [9,3,7] → [3,7,9]      isort "abracadabra" → "aaaaabbcdrr"



-- 9. E) Test with quickCheck

-- p_isort = for any list xs, isort xs is a sorted list


-- 9. F) Estimate efficiency


------------------------------------------------------------------------
-- 10. [DONE] Using iterate                                           --
------------------------------------------------------------------------

{-
+-------+--------------------+
| NOTES | iterate            |
+-------+--------------------+
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

iterate f x  → [x, fx, f (f x), f (f (fx))), ... ]


iterate (+1) 0 → [0,1,2,3,4, ...]
iterate (+2) 1 → [1,3,5,7,9, ...]



------- Exercise
A) Geometric progression

geometric 1  2 → [ 1,   2,  4...]
geometric 10 3 → [10,  30, 90...]

geometric a  b → [ a, a*b, a*b²...]


C) Multiples

multiplesOf x
multiplesOf 2 → [0,2,4,8...]


-}

-- 10. A) Geometric progression

-- Idea with list comprehension
-- geometric x y = [ a*b | a <- x, b <- [y, y*y, y*y*y..]


geometric x y = iterate (*y) x


-- 10. B) Property check

p1_geometric x r = x > 0 && r > 0 ==>
                        and [div z y == r | (y,z) <- zip xs (tail xs)]
                            where xs = take 100 (geometric x r)

{-
X and R must be greater than 0
We take 100 elements of a geometric progression and call it xs, then we check if the division of z and y is r.
Where z and y are calculated by the zip of xs and the tail of xs.
-}

-- 10. C) Multiples

multiplesOf x = iterate (+x) 0

-- 10. D) Power

powersOf x = iterate (*x) 1


------------------------------------------------------------------------
-- 11. [TODO 11.B] Testing concatenation                                   --
------------------------------------------------------------------------

{-
+-------+--------------------+
| NOTES | concat             |
+-------+--------------------+

The concat or ++ operator are defined recursively as:

infixr 5 ++
(++) :: [a] -> [a] -> [a]
[]      ++ ys = ys
(x:xs)  ++ ys = x : (xs++ys)    

-}

-- 11. A) quickCheck empty list concat

p_concat xs = xs ++ [] == xs


-- 11. B) Induction emtpy list is possible to use for concatenation

-- p_concat2 = [] ++ [] = []
-- p_concat3 xs = if xs ++ [] = xs then (x:xs) ++ [] =  (x:xs)


------------------------------------------------------------------------
-- 12. [TODO] Efficiency of the associative property                  --
------------------------------------------------------------------------

{-
+-------+--------------------+
| NOTES |                    |
+-------+--------------------+
-}


------------------------------------------------------------------------
-- 13. [TODO 13.C, D] Remove duplicates                                       --
------------------------------------------------------------------------

{-
+-------+--------------------+
| NOTES | nub                |
+-------+--------------------+

nub [1,3,1,2,7,2,9] → [1,3,2,7,9]

+-------+--------------------+
| NOTES | elem               |
+-------+--------------------+

elem checks if the item is in a list

5 `elem` [1,2,5,9] → True
'1' `elem` "0000"  → False

+-------+--------------------+
| NOTES | all                |
+-------+--------------------+

Checks if all the elements of the list hold the condition

all (>10) [100, 50, 20] → True
all (=='0') "0000"      → True
all even  [1,2,3,4]     → False


-}


-- 13. A) Define nub

nub' :: (Eq a) => [a] -> [a]
nub' ys = nub'' ys []                                 -- aux. list
    where
        nub'' [] _          = []                      -- base case
        nub'' (x:xs) ls
            | elem x ls     = nub'' xs ls             -- elem is in list
            | otherwise     = x : nub'' xs (x:ls)     -- elem not found


-- 13. B) quickCheck

p_nub' xs = nub xs == nub' xs

-- 13. C) Correct property

p_necessary xs = allDifferent (nub' xs)

{-

-}

-- 13. D) All elements in list ys are elements of list xs

allIn :: (Eq a) => [a] -> [a] -> Bool
ys `allIn` xs = all (`elem` xs) ys

-- "011001"  `allIn` "01" → True
-- "01A1001" `allIn` "01" → False

-- p_noRepetition ys xs = 

------------------------------------------------------------------------
-- 14. [DONE] Generate all possible Binary Numbers                    --
------------------------------------------------------------------------

{-
Desired Output:

bin 0 => [""]
bin 1 => ["0", "1"]
bin 2 => ["00", "01", "10", "11"]

+-------+----------------+
| NOTES | The $ operator |
+-------+----------------+
 
$ is an infix operator often seen in Haskell code. It applies the function on its left to the value on its right.

With:
f $ g $ h x  =  f (g (h x))

Without:
f g h x = ((f g) h) x

Read More: https://wiki.haskell.org/$

+-------+----------------+
| NOTES | Reverse        |
+-------+----------------+

Basically reverses a list
[a] -> [a]

reverse [12, 123] -> [123, 12]

+-------+----------------+
| NOTES | fst            |
+-------+----------------+

Takes the first element
fst :: (a,b) -> a

fst (12, 23) -> 12


-}


-- Generate list of possible binary numbers
genBin :: Int -> Int -> [Int]
genBin numdig k = reverse $ fst $ foldr step ([], k) power2
    where step exponent (genBin, remainder) =
            let (digit, newRemainder) = remainder `divMod` exponent
            in (digit : genBin, newRemainder)
          power2 = [2^n | n <- [0..(numdig-1)]]

-- Get combination from list
combination :: Int -> [[Int]]
combination n = map (genBin n) [0..(2^n)-1]



------------------------------------------------------------------------
-- 15. [TODO]                                                         --
------------------------------------------------------------------------

-- 15. A) varRep

{-
Desired Output:
    varRep 0 "abc" → [""]
    varRep 1 "abc" → ["a","b","c"]
    varRep 2 "abc" → ["aa","ab","ac","ba","bb","bc","ca","cb","cc"]
-}


-- 15. B) quickCheck

-- p_varRep m xs = m>=0 && m<=5 && n<=5 && allDifferent xs ==>
--                     len vss == n^m
--                     && allDifferent vss
--       where
--          vss = varRep m xs
--          n = len xs
--          len :: [a] -> Integer
--          len xs = fromIntegral (length xs)


------------------------------------------------------------------------
-- 16. [DONE] Factorial of natural numbers                            --
------------------------------------------------------------------------

{-
Desired output:

take 10 facts → [1,1,2,6,24,120,720,5040,40320,362880]

If we keep n! and (n+1)! values for (n+1)! and (n+2)! is addition and multiplication

(n+1)! = (n+1)*n!
-}

facts = [ fact x | x <- [0..]]
    where
        fact 0 = 1
        fact x = fact (x-1) *x

