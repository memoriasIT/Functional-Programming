-----------------------------------------------------
--               LIST COMPREHENSION                --
-----------------------------------------------------


multiply2 n = [x*2 | x <- [1..  n]]
-- *Main> multiply2 3
-- [2,4,6]


-- !! substitute mult por x*2 y usar list comprehension
-- multgrt10 n = [mult | x <- [1.. n], mult > 10]
--     where mult = x*2

multgrt10 n = [x*2 | x <- [1.. n], x*2 > 10]
-- multgrt10 6 --> [12]


tupleGen = [(i,j) | i <- [1,2],
                    j <- [1..4] ]

-- Output: [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4)]


-- Use list comprehension in function as a list 
tupleTake = take 10 [ (i,j) | i <- [1,2], 
                              j <- [1..] ]

-- Output: [(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,10)]



nestedTuples = take 5 [ [ (i,j) | i <- [1,2] ] | j <- [1..] ]

-- Output: [[(1,1),(2,1)],[(1,2),(2,2)],[(1,3),(2,3)],[(1,4),(2,4)],[(1,5),(2,5)]]


boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]  
-- boomBangs [1, 2, 12, 13] --> ["BOOM!","BANG!"]



length' xs = sum [ 1 | _ <- xs]





-----------------------------------------------------
--             REPLACE AND ITERATE                 --
-----------------------------------------------------

-- Replace numbers to letters
valueOf :: Int -> Char
valueOf 0 = 'x'
valueOf 1 = 'y'
valueOf _ = 'z'

-- For each element of list of list return list of string
listValues :: [[Int]] -> [String]
listValues = map (map valueOf)

-- Print
printValues :: [[Int]] -> IO ()
printValues = putStrLn . unlines . listValues

list1 :: [[Int]]
list1 = [[1,0,0],[0,1,0],[0,0,1]]



-----------------------------------------------------
--             RECURSIVE DEFINITIONS               --
-----------------------------------------------------


{-
-- split [1,2,3,4,5,6] → ([1,3,5], [2,4,6])
-- Take an element to first set, next to second set, infinitely
-- Complexity is niterations * O(1) = O(n)
split :: [a] -> ([a], [a])
split xs = aux xs True [] [] -- xs - original set | True - bool to first or second set | two sets starting without any elements
    where
        aux []      b xs ys = (xs, ys)
        aux (z:zs)  b xs ys
            | b == True     = aux zs (not b) (z:xs) ys
            | b == False    = aux zs (not b) xs     (z:ys) 

-- split [1,2,3,4,5,6] → ([5,3,1],[6,4,2])

-- split whithout reverse
-- option A (not recommended, complexity is very bad O(n²) niterations * O(n))
split' :: [a] -> ([a], [a])
split' xs = aux xs True [] [] -- xs - original set | True - bool to first or second set | two sets starting without any elements
    where
        aux []      b xs ys = (xs, ys)
        aux (z:zs)  b xs ys
            | b == True     = aux zs (not b) (xs (++) z)    ys
            | b == False    = aux zs (not b) xs             (ys (++) z)
-- option B : reverse the elements at the end (O(n) efficient)
split'' :: [a] -> ([a], [a])
split'' xs = aux xs True [] [] -- xs - original set | True - bool to first or second set | two sets starting without any elements
    where
        aux []      b xs ys = (reverse xs, reverse ys)
        aux (z:zs)  b xs ys
            | b == True     = aux zs (not b) (z:xs) ys
            | b == False    = aux zs (not b) xs     (z:ys) 


split''' :: [a] -> ([a], [a])
split''' [] = ([], [])
split''' (x:y:zs)
    where
        (xs, ys) = split' zs



split4  :: [a] -> ([a], [a])
split4  []       = ([], [])
split4  [x]      = ([x], [])
split4  (x:y:zs) = (x:xs, y:ys)
    where
        (xs, ys) = split'' zs
    
-}

split :: [a] -> ([a], [a])
split xs = aux xs True [] []
    where
        --n iterations = O(1) = O(n) + O(n) + O(n)
        aux []              b xs ys = (reverse xs, reverse ys)
        aux (z : zs)        b xs ys
            |   b           = aux zs (not b) (z:xs) ys
            |   otherwise   = aux zs (not b) xs (z:ys)
 
 
-- Example 2
split' :: [a] -> ([a], [a])
split' [] = ([], [])
split' [x] = ([x], [])
split' (x:y:zs) = (x:xs, y:ys)
     where
        (xs, ys) = split' zs



-- removeDup [1,2,3,1,7,2,5] → [1,2,3,7,5]
removeDup :: (Eq a) => [a] -> [a]
removeDup xs = aux xs []
    where
        aux (x:xs) ys
            | elem x ys = aux xs ys
            | otherwise = aux xs (x:ys)



removeDup' :: (Eq a) => [a] -> [a]
removeDup' [] = []
removeDup' (x:xs) = x : removeDup' [y  | y <- xs, y /= x]



removeDup'' :: (Eq a) => [a] -> [a]
removeDup'' [] = []
removeDup'' (x:xs) = if elem x ys then ys else x:ys
    where
        ys = removeDup'' xs
















