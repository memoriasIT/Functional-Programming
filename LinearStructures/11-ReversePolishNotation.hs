
{-
(1+2)*(3+4)
1 2 + 3 4 + *

-}

import Data.Char(ord)
type Op = Int -> Int -> Int
data Stack a = Empty | Node a (Stack a)
    deriving Show


empty :: Stack a
empty = Empty

isEmpty :: Stack a -> Bool
isEmpty Empty = True
isEmpty _     = False

push :: a -> Stack a -> Stack a
push x s     = Node x s

pop :: Stack a -> Stack a
pop Empty       = error "Can't pop on empty stack"
pop (Node x s1) = s1

top :: Stack a -> a
top Empty      = error "Can't top on empty stack"
top (Node x s) = x

-- Reverse polish notation
rpn :: [Char] -> Int
rpn str = aux str Empty 
    where
        aux :: [Char] -> Stack Int -> Int
        aux "" stck     = top stck 
        aux (x:xs) stck
            | elem x ['0'..'9'] = aux xs (push (ord x) stck)
            | elem x "+-*/"     = aux xs (push (calc x (top stck) (top(pop stck))) (pop(pop stck)))
            | otherwise         = error "Data not valid"
        calc :: Char -> Int -> Int -> Int
        calc op int1 int2 = (toOp op) int1 int2
        toOp :: Char -> Op 
        toOp '+' = (+)
        toOp '-' = (-)
        toOp '*' = (*)
        toOp '/' = div 











