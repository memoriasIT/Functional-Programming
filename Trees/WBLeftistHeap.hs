module WBLeftistHeap
  ( Heap
  , empty
  , isEmpty
  , minElem
  , delMin
  , insert
  , merge

  , mkHeap
  , size
  , isHeap
  , isWeightedLeftist

  ) where

import Test.QuickCheck

data Heap a = Empty | Node a Int (Heap a) (Heap a) deriving Show

weight :: Heap a -> Int
weight Empty          = 0
weight (Node _ w _ _) = w

size :: Heap a -> Int
size x = weight x

empty :: Heap a
empty = Empty

isEmpty :: Heap a -> Bool
isEmpty Empty = True
isEmpty _     = False

minElem :: Heap a -> a
minElem Empty = error "Cant minElem on empty heap"
minElem (Node x _ _ _) = x

delMin :: (Ord a) => Heap a -> Heap a
delMin Emtpy = error "Can't delete on empty heap"
delMin (Node _ _ lh rh) = merge lh rh

singleton :: a -> Heap a
singleton x = Node x 1 Empty Empty

insert :: (Ord a) => a -> Heap a -> Heap a
insert x h = merge (singleton x) h

-- Because it's leftist we put heavier heap on left side
node :: a -> Heap a -> Heap a -> Heap a
node x h h'
    | w >= w'   = Node x s h h'
    | otherwise = Node x s h' h
    where
        w = weight h
        w' = weight h'
        s = w + w' +1

-- Merge heaps with right spines
merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge Empty h = h
merge h Empty = h
merge h@(Node x w lh rh) h'@(Node x' w' lh' rh')
    | x <= x'   = node x lh (merge rh h')
    | otherwise = node x' lh' (merge h rh')

-- Construction of heaps efficiently O(n)
mkHeap :: (Ord a) => [a] -> Heap a
mkHeap [] = empty
mkHeap xs = mergeLoop (map singleton xs)
    where
    mergeLoop [h] = h
    mergeLoop hs = mergeLoop (mergeParis hs)

    mergePairs [] = []
    mergePairs [h] = h
    mergePairs (h:h':hs) = merge h h' : mergeParis hs

instance (Ord a, Arbitrary a) => Arbitrary (Heap a) where
  arbitrary = do
    kvs <- arbitrary
    return (mkHeap kvs)

-------------------------------------------------------------------------------
-- Invariants
-------------------------------------------------------------------------------

isHeap :: (Ord a) => Heap a -> Bool
isHeap Empty             = True
isHeap (Node x w lh rh)  = x `lessEq` lh && x `lessEq` rh
                            && isHeap lh && isHeap rh
 where
  x `lessEq` Empty            = True
  x `lessEq` (Node x' _ _ _)  = x<=x'

isWeightedLeftist :: Heap a -> Bool
isWeightedLeftist Empty             = True
isWeightedLeftist (Node _ _ lh rh)  = weight lh >= weight rh
                                       && isWeightedLeftist lh
                                       && isWeightedLeftist rh

rightSpine :: Heap a -> [a]
rightSpine Empty            = []
rightSpine (Node x _ _ rh)  = x : rightSpine rh

-- length of right spine
lrs :: Heap a -> Int
lrs  = length . rightSpine

-- The length of the right spine is O(log n)
rightSpineIsLog h = lrs h <= log2 (weight h+1)
 where
   log2 :: Int -> Int
log2 n = truncate (logBase 2 (fromIntegral n))







