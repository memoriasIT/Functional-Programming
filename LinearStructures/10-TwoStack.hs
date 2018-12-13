
------------------------------------------------------------------------
-- 10. [DONE] Implement Dijkstra two stack algorithm                  --
------------------------------------------------------------------------

{-
Use case: evaluate arithmetic expression
Valid expression:
    "((2+5)*(8-(2*3)))"

Two Stacks -> Operators and Values Stacks
Processed from left to right

    - Value encountered     → push to values stack
    - Operator encountered  → push to operators stack
    - Left parenthesis      → ignored
    - Right parenthesis     → operator and two values popped, result pushed to values

-}


import Data.Char(ord)
type Op = Int -> Int -> Int
data Stack a = Empty | Node a (Stack a)
    deriving Show


-- push
push :: a -> Stack a -> Stack a
push x s1 = Node x (s1)

-- pop
pop :: Stack a -> Stack a
pop Empty          = Empty
pop (Node x xs)    = xs 

-- top
top :: Stack a -> a
top Empty          = error "Can't top an empty stack" 
top (Node x xs)    = x

-- empty
empty :: Stack a
empty = Empty

-- isEmpty
isEmpty :: Stack a -> Bool
isEmpty Empty = True
isEmpty _     = False


--dijkstra
dijkstra :: [Char] -> Int
dijkstra "" = 0
dijkstra x = aux x Empty Empty
    where
    aux :: [Char] -> Stack Int -> Stack Char -> Int
    aux (x:xs) sint sop
        | elem x ['0'..'9']       = aux xs (push (ord x) sint) sop
        | elem x "+-*/"           = aux xs sint (push x sop)
        | x == '('                = aux xs sint sop
        | x == ')'                = aux xs (push (calc (top sop) (top sint) (top (pop sint))) (pop(pop sint))) (pop sop)
    calc :: Char -> Int -> Int -> Int
    calc op int1 int2 = (toOp op) int1 int2
    toOp :: Char -> Op
    toOp '+' = (+)
    toOp '-' = (-)
    toOp '*' = (*)
    toOp '/' = div





