data TreeB a = EmptyB | NodeB a (Tree a) (Tree a) deriving Show

sumB :: (Num a) => TreeB a -> a
sumB Empty = 0
sum (Node x lt rt) = x + sumB lt + sumB rt

heightB :: TreeB a -> a
heightB EmptyB = 0
heightB (Node x lt rt) = 1 + max (height lt) (heightB rt)
