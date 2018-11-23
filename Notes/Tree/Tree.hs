{-

  FUNCTION SUM
----------------
sum [ 1 , 2 , 3]
1+2+3 = 6

    CONCAT
----------------
Concatenates a list of list into list

concat [ [1,2,3], [4,5]]
[1,2,3,4,5]

    MAXIMUM
----------------
maximum [2, 7, 4]
7
-}


data Tree a = Empty | Node a [Tree a] deriving Show

tree1 :: Tree Int
tree1 = Node 1 [Node 2 [Node 3 []
                       , Node 4 []
                       , Node 5 []
                       ]
                , Node 6 [ Node 7 [] ]
                ]

sumT :: (Num a) => Tree a -> a
sumT Empty      = 0
sum (Node x ts) = x + sum [ sumT t | t <- ts ]


toList :: Tree a -> [a]
toList Empty = []
toList (Node x ts) = x concat [ toList t | t <- ts]


heightT :: Tree a -> Int
heightT Emtpy = 0
heightT (Node x []) = 1
heightT (Node x ts) = 1 + maximum [ heightT t | t <- ts ]
