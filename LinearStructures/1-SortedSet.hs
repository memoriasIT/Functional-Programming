------------------------------------------------------------------------
-- 1. [DONE] Implement a Sorted Set                                   --
------------------------------------------------------------------------
{-

-- TODO: implement instances and set operations


--------{ MODULE STRUCTURE }------------

// module + dir structure

    module DataStructures.Set.SortedLinearSet 

// exports
      ( Set
      , empty
      ) where

// the proper module (imports + data structure) 
    import Data.List(intercalate)
    import Test.QuickCheck
    
    data Set a  = Empty | Node a (Set a)
    
    empty :: Set a
    ...

--------{ INSTANCES }-----------
// Model
instance «preconditions» => Class «type» where
  «method» = «definition»


// Example
instance Show Prediction where
  show (Prediction a b c) = show a ++ "-" ++ show b ++ "-" ++ show c

-}


module DataStructures.Set.SortedLinearSet
  ( Set
  , empty
  , isEmpty
  , size
  , insert
  , isElem
  , delete
-- 
--   , fold
-- 
--   , union
--   , intersection
--   , difference  
  ) where

import Data.List(intercalate)
import Test.QuickCheck


-- Set

data Set a = Empty | Node a (Set a)

-- empty

empty :: Set a
empty = Empty


-- isEmpty

isEmpty :: Set a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- size
size :: Set a -> Int
size Empty    = 0
size (Node _ s) = 1 + size s

-- insert

insert :: (Ord a) => a -> Set a -> Set a
insert y Empty      = Node y (Empty)
insert y (Node x s)
    | y <  x        = Node y (Node x s)
    | y == x        = Node x s 
    | otherwise     = Node x (insert y s) 

-- isElem

isElem :: (Ord a) => a -> Set a -> Bool
isElem x Empty  = False 
isElem x (Node y s)
    | x < y     = False
    | x == y    = True
    | otherwise = isElem x s

-- delete

delete :: (Ord a) => a -> Set a -> Set a
delete x Empty      = Empty
delete x (Node y s)
    | x <  y        = Node y s
    | x == y        = s
    | otherwise     = Node y (delete x s)


-- fold

fold :: (a -> b -> b) -> b -> Set a -> b
fold f z = fun
    where
    fun Empty = z
    fun (Node x s) = f x (fun s)

-- union

-- 1ST IMPLEMENTATION (NON LINEAR)
-- Because fold does n times a search in the set m this is not a good implementation
-- union :: (Ord a) => Set a -> Set a -> Set a
-- union x y = fold insert x y

-- 2ND IMPLEMENTATION (LINEAR O(n+m))
-- Because the elements are in order we can do a linear implementation
union :: (Ord a) => Set a -> Set a -> Set a
union s1 Empty = s1
union (Node x s1) (Node y s2)
    | x == y    = Node x (union s1 s2)
    | x <  y    = Node x (union s1 (Node y s2))
    | otherwise = Node y (union s2 (Node x s1))


-- intersection

-- 1ST IMPLEMENTATION (NON LINEAR)
-- intersection :: (Ord a) => Set a -> Set a -> Set a
-- intersection s s'  = fold (\x inter -> if isElem x s then insert x inter else inter) empty s'  

-- 2ND IMPLEMENTATION (LINEAR O(n+m))
-- Because the elements are in order we can do a linear implementation
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection _ Empty               = Empty
intersection Empty _               = Empty 
intersection (Node x s1) (Node y s2)
    | x == y        = Node x (intersection s1 s2)
    | x <  y        = intersection s1 (Node y s2)
    | x >  y        = intersection (Node x s1)  s2


-- difference  

-- 1ST IMPLEMENTATION (NON LINEAR)
-- difference :: (Ord a) => Set a -> Set a -> Set a
-- difference x y = fold delete x y

-- 2ND IMPLEMENTATION (LINEAR O(n+m))
-- Because the elements are in order we can do a linear implementation
difference :: (Ord a) => Set a -> Set a -> Set a
difference x Empty = x
difference Empty x = Empty
difference (Node x s1) (Node y s2)
    | x == y    = difference s1 s2
    | x <  y    = Node x (difference s1 (Node y s2))     
    | x >  y    = difference (Node x s1) s2


-- Subset

-- 1ST IMPLEMENTATION (Doesn't stop)
-- subset :: (Ord a) => Set a ->  Set a -> Bool
-- subset x Empty = True
-- subset Empty x = False
-- subset x y     = union x y == x

-- 2ND IMPLEMENTATION (Due to order we stop)
subset :: (Ord a) => Set a ->  Set a -> Bool
subset x Empty = True
subset Empty x = False
subset (Node x s1) (Node y s2)
    | x == y    = subset s1 s2
    | x <  y    = subset s1 (Node y s2)
    | x >  y    = False


-- Show

instance (Show a) => Show (Set a) where
  show s  = "SortedLinearSet(" ++ intercalate "," (aux s) ++ ")"
    where
      aux Empty       = []
      aux (Node x s)  = show x : aux s

-- Set equality

instance (Eq a) => Eq (Set a) where
  Empty      == Empty         = True
  (Node x s) == (Node x' s')  = x==x' && s==s'
  _          == _             = False




-- Arbitrary (generate random sets)

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary  = do
      xs <- listOf arbitrary
      return (foldr insert empty xs)
