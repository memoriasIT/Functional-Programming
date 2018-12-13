data Deque a = DQ [a] [a] 
    deriving Show

-- example
q1 :: Deque Int
q1 = (DQ [1,2,3,4] [8,7,6,5])

-- empty
empty :: Deque a
empty = DQ [] [] 

-- isEmpty
isEmpty :: Deque a -> Bool
isEmpty (DQ [] []) = True
isEmpty _          = False

-- Add element at front 
prepend :: a -> Deque a -> Deque a
prepend x (DQ s1 s2)  = DQ (x:s1) s2


-- Add element at back
append :: a -> Deque a -> Deque a
append x (DQ s1 s2) = DQ s1 (x:s2)

-- Get first element of DQ
first :: Deque a -> a
first (DQ [] _) = error "Can't extract first element on empty deque"
first (DQ (x:xs) _) = x

-- Get last element of DQ
lastq :: Deque a -> a
lastq (DQ _ []) = error "Can't extract last element on empty deque"
lastq (DQ _ (x:xs)) = x


-- Remove last element
remlast :: Deque a -> Deque a
remlast (DQ sq (x:xs)) = DQ sq xs


-- Remove first element
remfirst :: (Eq a) => Deque a -> Deque a
remfirst (DQ [] []) = error "Can't remove element on empty deque"
remfirst (DQ [] s2) = remfirst (organize (DQ [] s2))
remfirst (DQ (x:xs) s2) = DQ xs s2

-- If DQ [] y we organize the deque
organize (DQ s1 (x:xs))
    | length (x:xs) == 1    = DQ  [x] []
    | (length s1) == (length (x:xs)) ||  (length s1)+1 == (length (x:xs)) = DQ s1 (x:xs)
    | length (x:xs) == 2    = DQ (s1++xs) [x]
    | otherwise             = organize (DQ (s1 ++ [last xs]) (remlastlist (x:xs)))
    where
        remlastlist :: (Eq a) => [a] -> [a]
        remlastlist [] = []
        remlastlist (x:xs)
            | x == (last (x:xs)) = []
            | otherwise          = x:(remlastlist xs)





