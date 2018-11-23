import DataStructures.Stack.LinearStack

-- Create Stack
s1 :: Stack Int
s1 = push 3 (push 2(push 1 empty))

-- Work with Datatype
size :: Stack a -> Int
size s
    | isEmpty s = 0
    | otherwise = 1 + size (pop s)



-- Because it is a abstract datatype we don't care about the definition of the datatype.
