{-

[TODO] copyelem

In computer science and mathematics, the Josephus problem (or Josephus permutation) is a
theoretical problem related to a certain counting-out game. There are people standing in a circle
waiting to be executed. After the first person is executed, certain number of people (k) are skipped
and one person is executed. Then again, same number of people are skipped and a person is
executed. The elimination proceeds around the circle (which is becoming smaller and smaller as the
executed people are removed), until only the last person remains, who is given freedom.
The problem is named after Flavius Josephus, a Jewish historian living in the 1st century. According
to Josephus' account of the siege of Yodfat, he and his 40 comrade soldiers were trapped in a cave,
the exit of which was blocked by Romans. They chose suicide over capture and decided that they
would form a circle and start killing themselves using a step of three. Josephus states that by luck or
maybe by the hand of God (modern scholars point out that Josephus was a well-educated scholar
and predicted the outcome), he and another man remained the last and gave up to the Romans

Using a queue, develop an algorithm to simulate Josephus problem. Your function should take the
initial number of people (n) and the number of persons to skip in each turn (k). Assuming that
people are identified by numbers in 0 to n-1, your function should return a list with people
identities as they are executed, so that last element in the list is the person freed.


-}

data Queue = Empty | Node a (Queue a)
    deriving Show

empty :: Queue
empty   = Empty

isEmpty :: Queue a -> Bool
isEmpty Empty = True
isEmpty _     = False 

enqueue :: a -> Queue a -> Queue a
enqueue x q1 = Node x q1

first :: Queue a -> a
first Empty       = error "Cannot first on empty Queue"
first (Node x q1) = x

dequeue :: Queue a -> Queue a
dequeue Empty       = error "Cannot dequeue on empty Queue"
dequeue (Node x q1) = q1


josephus :: Int -> Int -> [Int] 
josephus n k = aux n k Empty Empty 1
    where
    aux 0 _ _ _   = error "There are no people to murder"
    aux _ 0 _ _   = error "Jump must be greater than 0"
    aux n k s1 s2 rep
        | n == 0      = (toList s2)
        | s1 == Empty = [1..n]
        | otherwise   = aux (n-1) k (copyelem (k*(rep+1)) s1) s2
    
    copyelem k (Node x xs) = auxcopy k (Node x xs) rep 
       [TODO] 



instance (Eq a) => Eq (Queue a) where
    Empty       == Empty             = True
    (Node x xs) == (Node y ys) = x == y && xs == ys
    _           ==  _          = False

instance (Show a) => Show (Queue a) where
    show q = "LinearQueue(" ++ intercalate "," (aux q) ++ ")"
        where 
            aux Empty      = []
            aux (Node x q) = show x : aux q



