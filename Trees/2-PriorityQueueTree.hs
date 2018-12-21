
-- HOW TO INSERT IF THEY ARE NOT IN ORDER


data PQT a = Empty
            | Node a (PQT a) (PQT a)
            deriving Show

empty ::  PQT a
empty = Empty

isEmpty :: PQT a -> Bool
isEmpty Empty = True
isEmtpy _     = False

size :: PQT a -> Int
size Empty = 0
size (Node _ lt rt) = 1 + size lt + size rt

enqueue :: a -> Queue a -> Queue a
enqueue x Empty = Node x Empty Empty
enqueue x (Node y  lt rt) 
    | lt == Empty    = Node y x rt
    | rt == Empty    = Node y lt x
    | 



