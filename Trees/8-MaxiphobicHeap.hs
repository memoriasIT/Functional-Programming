import DrawTrees
import Test.QuickCheck


data Heap a  = Empty | Node a Int (Heap a) (Heap a) deriving Show

-- number of elements
weight :: Heap a -> Int
weight Empty           = 0
weight (Node _ w _ _)  = w

size :: Heap a -> Int
size = weight

empty :: Heap a
empty  = Empty

isEmpty :: Heap a -> Bool
isEmpty Empty  = True
isEmpty _      = False

minElem :: Heap a -> a
minElem Empty           = error "minElem on empty heap"
minElem (Node x _ _ _)  = x

delMin :: (Ord a) => Heap a -> Heap a
delMin Empty             = error "delMin on empty heap"
delMin (Node _ _ lh rh)  = merge lh rh

singleton :: a -> Heap a
singleton x = Node x 1 Empty Empty

insert :: (Ord a) => a -> Heap a -> Heap a
insert x h = merge (singleton x) h


-- puts always heavier heap on left side
node :: a -> Heap a -> Heap a -> Heap a
node x h h' = Node x s h' h

 where
   w = weight h
   w' = weight h'
   s = w + w' + 1

mayor:: Heap a->Heap a->Heap a->[Heap a]
mayor x y z
        |(weight x >= weight y) && (weight x >= weight z) = [x , y, z]
        |(weight y >= weight x) && (weight y >= weight z) = [y , x, z]
        |otherwise = [z , x, y]


merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge Empty h'     = h'
merge h     Empty  = h
merge h@(Node x w lh rh) h'@(Node x' w' lh' rh')
 | x <= x'         = node x ((mayor lh rh h')!!0) (merge ((mayor lh rh h')!!1) ((mayor lh rh h')!!2))  -- note that recursive calls use right side heap
 | otherwise       = merge h' h


h :: Heap Int
h = insert 5 (insert 4 (insert 6 (insert 2 (insert 8 (insert 1 (insert 5 Empty))))))

pretty :: (Show a) => Heap a -> IO ()
pretty t  = putStrLn (unlines xss)
 where
   (xss,_,_,_) = pprint' t

pprint' Empty                   =  ([], 0, 0, 0)
pprint' (Node x h Empty Empty)  =  ([s], ls, 0, ls-1)
  where
    s = show x
    ls = length s
pprint' (Node x h l r)          =  (resultLines, w, lw'-swl, totLW+1+swr)
  where
    nSpaces n = replicate n ' '
    nBars n = replicate n '_'
    -- compute info for string of this node's data
    s = show x
    sw = length s
    swl = div sw 2
    swr = div (sw-1) 2
    (lp,lw,_,lc) = pprint' l
    (rp,rw,rc,_) = pprint' r
    -- recurse
    (lw',lb) = if lw==0 then (1," ") else (lw,"/")
    (rw',rb) = if rw==0 then (1," ") else (rw,"\\")
    -- compute full width of this tree
    totLW = maximum [lw', swl,  1]
    totRW = maximum [rw', swr, 1]
    w = totLW + 1 + totRW
{-
A suggestive example:
     dddd | d | dddd__
        / |   |       \
      lll |   |       rr
          |   |      ...
          |   | rrrrrrrrrrr
     ----       ----           swl, swr (left/right string width (of this node) before any padding)
      ---       -----------    lw, rw   (left/right width (of subtree) before any padding)
     ----                      totLW
                -----------    totRW
     ----   -   -----------    w (total width)
-}
    -- get right column info that accounts for left side
    rc2 = totLW + 1 + rc
    -- make left and right tree same height
    llp = length lp
    lrp = length rp
    lp' = if llp < lrp then lp ++ replicate (lrp - llp) "" else lp
    rp' = if lrp < llp then rp ++ replicate (llp - lrp) "" else rp
    -- widen left and right trees if necessary (in case parent node is wider, and also to fix the 'added height')
    lp'' = map (\s -> if length s < totLW then nSpaces (totLW - length s) ++ s else s) lp'
    rp'' = map (\s -> if length s < totRW then s ++ nSpaces (totRW - length s) else s) rp'
    -- first part of line1
    line1 = if swl < lw' - lc - 1 then
                nSpaces (lc + 1) ++ nBars (lw' - lc - swl) ++ s
            else
                nSpaces (totLW - swl) ++ s
    -- line1 right bars
    lline1 = length line1
    line1' = if rc2 > lline1 then
                line1 ++ nBars (rc2 - lline1)
             else
                line1
    -- line1 right padding
    line1'' = line1' ++ nSpaces (w - length line1')
    -- first part of line2
    line2 = nSpaces (totLW - lw' + lc) ++ lb
    -- pad rest of left half
    line2' = line2 ++ nSpaces (totLW - length line2)
    -- add right content
    line2'' = line2' ++ " " ++ nSpaces rc ++ rb
    -- add right padding
    line2''' = line2'' ++ nSpaces (w - length line2'')
    resultLines = line1'' : line2''' : zipWith (\l r -> l ++ " " ++ r) lp'' rp''
