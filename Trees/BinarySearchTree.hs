--  ██████    ████████ ██████████
-- ░█░░░░██  ██░░░░░░ ░░░░░██░░░ 
-- ░█   ░██ ░██           ░██    
-- ░██████  ░█████████    ░██    
-- ░█░░░░ ██░░░░░░░░██    ░██    
-- ░█    ░██       ░██    ░██    
-- ░███████  ████████     ░██    
-- ░░░░░░░  ░░░░░░░░      ░░     
--
-- Binary Search Trees

-- mkBST?
-- Traversal
module BinarySearchTree where

import Data.Maybe(isJust)
-- True if Just _ else False

data BST a = Empty
            | Node a (BST a) (BST a)
            deriving Show

------------------------------------------------------------------------
-- BASE FUNCTIONS                                                     --
------------------------------------------------------------------------

empty :: BST a
empty = Empty

isEmpty :: BST a -> Bool
isEmpty Empty = True
isEmpty _     = False

size :: BST a -> Int
size Empty = 0
size (Node _ lt rt) = 1 + size lt + size rt


------------------------------------------------------------------------
-- SEARCH                                                             --
------------------------------------------------------------------------
search :: (Ord a) => a -> BST a -> Maybe a
search x Empty = Nothing
search x (Node y lt rt)
    | x < y     = search x lt
    | x > y     = search x rt
    | otherwise = Just x

isElem :: (Ord a) => a -> BST a -> Bool
isElem x t = isJust (search x t)



-------------------------------------------------------------------------
-- INSERTION                                                           --
-------------------------------------------------------------------------

insert :: (Ord a) => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node y lt rt)
    | x < y     = Node y (insert x lt) rt
    | x > y     = Node y lt (insert x rt)
    | otherwise = Node x lt rt

updateOrInsert :: (Ord a) => (a -> a) -> a -> BST a -> BST a
updateOrInsert f x Empty = Node x Empty Empty
updateOrInsert f x (Node y lt rt)
    | x < y     = Node y (updateOrInsert f x lt) rt
    | x > y     = Node y lt (updateOrInsert f x rt)
    | otherwise = Node (f x) lt rt

-- Insert with updateOrInsert
mkBTS :: (Ord a) => [a] -> BST a
mkBTS xs = foldl (flip insert) empty xs

--------------------------------------------------------------------------
-- DELETION                                                             --
--------------------------------------------------------------------------

delete :: (Ord a) => a -> BST a -> BST a
delete x Empty  = Empty
delete x (Node y lt rt)
    | x < y     = Node y (delete x lt) rt
    | x > y     = Node y lt (delete x rt)
    | otherwise = combine lt rt

combine :: BST a -> BST a -> BST a
combine Empty rt = rt
combine lt Empty = lt
combine lt rt    = Node x lt rt2
    where (x, rt2) = split lt

-- removes and returns minimum element from non-empty tree
split :: BST a -> (a, BST a)
split (Node x Empty rt) = (x, rt)
split (Node x lt rt)    = (x2, Node x lt2 rt)
    where (x2, lt2) = split lt

deleteMinim :: BST a -> BST a
deleteMinim Empty             = error "Cant deleteMinim on empty tree"
deleteMinim (Node x Empty rt) = rt
deleteMinim (Node x lt rt)    = Node x (deleteMinim lt) rt

deleteMaxim :: BST a -> BST a
deleteMaxim Empty             = error "Can't deleteMaxim on empty tree"
deleteMaxim (Node x lt Empty) = lt
deleteMaxim (Node x lt rt)    = Node x lt (deleteMaxim rt)


--------------------------------------------------------------------------
-- ORDER                                                                --
--------------------------------------------------------------------------

minim :: BST a -> a
minim Empty                   = error " Can't minim on empty tree"
minim (Node x Empty rt)       = x
minim (Node x lt rt)          = minim lt

maxim :: BST a -> a
maxim Empty = error "Can't maxim on empty tree"
maxim (Node x lt Empty)       = x
maxim (Node x lt rt)          = maxim rt

--------------------------------------------------------------------------
-- TRAVERSAL                                                            --
--------------------------------------------------------------------------

inOrder :: BST a -> [a]
inOrder t = aux t []
    where
        aux Empty   xs        = xs
        aux (Node x lt rt) xs = aux lt (x : aux rt xs)

preOrder :: BST a -> [a]
preOrder t = aux t []
    where
        aux Empty xs          = xs
        aux (Node x lt rt) xs = x : aux lt (aux rt xs)


postOrder :: BST a -> [a]
postOrder t = aux t []
    where
        aux Empty xs = xs
        aux (Node x lt rt) xs = aux lt (aux rt (x:xs))





















traversal :: ((b -> b) -> (b -> b) -> (b -> b) -> (b -> b)) ->
             (a -> b -> b) -> b -> BST a -> b
traversal order f z t  = aux t z
  where
    aux Empty           = id
    aux (Node x lt rt)  = order (f x) (aux lt) (aux rt)


foldInOrder :: (a -> b -> b) -> b -> BST a -> b
foldInOrder  = traversal (\xf lf rf -> lf . xf . rf)

foldPreOrder :: (a -> b -> b) -> b -> BST a -> b
foldPreOrder  = traversal (\xf lf rf -> xf . lf . rf)

foldPostOrder :: (a -> b -> b) -> b -> BST a -> b
foldPostOrder  = traversal (\xf lf rf -> lf . rf . xf)

-------------------------------------------------------------------------------
-- Very general operation:
-- Searchs for x in tree:
--  If node with x' such that x==x' was found:
--    if (f (Just x')) returns Nothing, then x' is deleted from tree
--    if (f (Just x')) returns (Just y), then x' is replaced by y (must not alter order)
--  If node with x such that x==x' wasn't found:
--    if (f Nothing) returns Nothing, then same tree is returned
--    if (f Nothing) returns (Just y), then y is inserted into tree
-------------------------------------------------------------------------------

update :: (Ord a) => a -> (Maybe a -> Maybe a) -> BST a -> BST a
update x f Empty  =
  case f Nothing of
    Nothing -> Empty
    Just x' -> Node x' Empty Empty
update x f (Node x' lt rt)
  | x<x'          = Node x' (update x f lt) rt
  | x>x'          = Node x' lt (update x f rt)
  | otherwise     =
      case f (Just x') of
        Nothing  -> combine lt rt -- delete
        Just x'' -> if x' == x'' then Node x'' lt rt
                    else error "update cannot modify order of element"

-- Many operations can be implemented by using this one:
insert' :: (Ord a) => a -> BST a -> BST a
insert' x  = update x $ maybe (Just x) -- insert if not found
                              (const (Just x)) -- replace with x if found


delete' :: (Ord a) => a -> BST a -> BST a
delete' x  = update x $ maybe Nothing -- do nothing if not found
                              (const Nothing) -- delete if found


-- If x is in tree, it's updated with f. Otherwise, x is inserted
updateOrInsert' :: Ord a => (a -> a) -> a -> BST a -> BST a
updateOrInsert' f x  = update x $ maybe (Just x) -- insert if not found
                                        (Just . f) -- update if found

-- If x' such that x==x' isn't in tree, x it's inserted.
-- If x' such that x==x' is in tree:
--  If p x' then x' is deleted from tree
--  otherwise, x' is updated with f
updateOrDeleteOrInsert :: Ord a => (a -> a) -> (a -> Bool) -> a -> BST a -> BST a
updateOrDeleteOrInsert f p x  = update x $ maybe (Just x)  -- insert if not found
                                                 deleteOrUpdate -- delete or update if found
 where
   deleteOrUpdate x'
    | p x'      = Nothing -- delete
    | otherwise = Just (f x') -- update








