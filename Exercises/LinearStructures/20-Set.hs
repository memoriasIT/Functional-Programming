data Set a = S [a]

empty :: Set a
empty = S []

isEmpty :: Set a -> Bool 
isEmpty (S []) = True
isEmpty _      = False

insert :: a -> Set a -> Set a
insert x (S xs) = (S (x:xs))

size :: Set a -> Int
size (S (x:xs)) = 1 +  size (S xs)

isElem :: (Eq a) => a -> Set a -> Bool
isElem x (S []) = False
isElem x (S (y:ys))
    | x == y    = True
    | otherwise = isElem x (S ys)

delete :: (Eq a) => a -> Set a -> Set a
delete x (S (y:ys))
    | x == y    = S ys
    | x /= y    = insert y (delete x (S ys))

fold :: (a -> b -> b) -> b -> Set a -> b
fold f z (St xs) = foldr f z xs


union :: Set a -> Set a -> Set a
union x y = fold insert x y

difference :: Set a -> Set a -> Set a
difference x y = fold delete x y

intersection :: Set a -> Set a -> Set a
intersection x y = fold (\x inter -> if isElem x s then insert x inter else inter) empty y

