data Stack a


-- Because it is a abstract datatype we don't care about the definition of the datatype.

empty:: Stack a

isEmpty :: Stack a -> Bool

push :: a -> Stack a -> Stack a

top :: Stack a -> a

pop :: Stack a -> Stack a



-- SEMANTICS OF STACK (if this is fullfuled this is a Stack)
-- Use quickCheck to see if we have a correct implementation of stack

-- An empty stack is empty
True ==> isEmpty empty

-- push returns a non-empty stack
True ==> not(isEmpty (push x s))

-- top returns last pushed element
True ==> top (push x s) == x

-- pop removes last pushed element
True ==> pop(push x s) == s

----------------------------------------------------------------------------------------------
--                        STACK IMPLEMENTATION WITH A RECURSIVE DATA TYPE                   --
----------------------------------------------------------------------------------------------

data Stack a = Empty | Node a (Stack a) deriving Show
{-         
           |     |       | 
           |     |       | 
           |     |       +---------------------------  Node (not empty, element on top of Stack)
           |     +-----------------------------------  Empty stack
           +-----------------------------------------  Polymorphism

-}



top :: Stack a -> a
top Empty           = error "top for empty stack"
top (Node x s)      = x


size :: Stack a -> Int
size Empty      = 0
size (Node x s) = 1 + size s





















