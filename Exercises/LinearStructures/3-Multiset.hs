------------------------------------------------------------------------
-- 3. [DONE] Bag - Set with multiple entries                          --
------------------------------------------------------------------------

module DataStructures.Bag
  ( empty
  , isEmpty
  , insert
  , ocurrences
  , delete
  ) where


data Bag a = Empty | Node a Int (Bag a)
    deriving (Show)
{-                        ^ ^   ^
 -                        | |   |
 -                        | |   +-------- Rest of Bag
 -                        | +------------ Number of elements
 -                        +-------------- Value of elem


[1,3,3,1,3,7,3,9,1,3,1]
Node 1 4 (Node 3 5 (Node 7 1 (Node 9 1 Empty)))

4 occurrences of 1, 5 occurrences of 3, 1 occurence of 7 and 1 ocurrence of 9.
-}


-- empty       - returns empty bag

empty :: Bag a
empty = Empty

-- isEmpty     - tests for emptinesss

isEmpty :: Bag a -> Bool
isEmpty Empty    =  True
isEmpty _        =  False

-- insert      - inserts new element in bag

insert :: (Ord a) => a -> Bag a -> Bag a
insert x (Node y n s) 
    | x < y     = Node x 1 (Node y n s)
    | x == y    = Node y (n+1) s
    | x > y     = Node y n (insert x s)


-- occurrences - return number of ocurrences of element

ocurrences :: (Ord a) => a -> Bag a -> Int
ocurrences x Empty = 0
ocurrences x (Node y n s)
    | x <  y    = 0
    | x == y    = n
    | x >  y    = ocurrences x s

-- delete      - removes 1 ocurrence or same bag if not included

delete :: (Ord a) => a -> Bag a -> Bag a
delete x Empty  = Empty
delete x (Node y n s)
    | x < y     = Node y n s
    | x == y    = if n == 0 then s else Node y (n-1) s
    | otherwise = Node y n (delete x s)



data Bag a = Empty | Node a Int (Bag a)

-- union

union :: (Ord a) => Bag a -> Bag a -> Bag a
union s1 Empty = s1
union (Node x xn s1) (Node y yn s2)
    | x == y    = Node x (xn + yn) (union s1 s2)
    | x <  y    = Node x xn (union s1 (Node y yn s2))
    | x >  y    = Node y yn (union s2 (Node x xn s1))

-- intersection

intersection :: (Ord a) => Bag a -> Bag a -> Bag a
intersection _ Empty    = Empty
intersection Empty _    = Empty
intersection (Node x xn s1) (Node y yn s2)
    | x == y    = if xn > yn then Node x xn (intersection s1 s2) else Node y yn (intersection s1 s2)
    | x <  y    = intersection s1 (Node y yn s2)
    | x >  y    = intersection (Node x xn s1) s2

-- difference

difference :: (Ord a) => Bag a -> Bag a -> Bag a
difference x Empty  = x
difference Empty x  = Empty
difference (Node x xn s1) (Node y yn s2)
    | (x == y && xn == yn) || (x == y && xn < yn)    = difference s1 s2
    | x == y && xn > yn                              = Node x (xn-yn) difference s1 s2
    | x <  y                                         = Node x xn (difference s1 s2)

-- subset

subset :: (Ord a) => Bag a -> Bag a -> Bool
subset Empty _ = True
subset _ Empty = False
subset (Node x xn s1) (Node y yn s2)
    | x == y    = subset s1 s2
    | x <  y    = subset s1 (Node y s2)
    | x >  y    = False



-- arbitrary
instance (Ord a, Arbitrary a) => Arbitrary (Bag a) where
    arbitrary = do
        xs <- listOf arbitrary
        return (foldr insert empty xs)
