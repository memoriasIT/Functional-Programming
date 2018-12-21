-- Just a general tree doing general things uwu
module GeneralTree where

data Tree a = Empty
            | Node a [Tree a]
            deriving Show

empty :: Tree a
empty = Empty

-- Boolean emptiness
isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- Size of tree
size :: Tree a -> Int
size Empty = 0
size (Node x ts) = 1 + sum [size t | t <- ts] 

-- Return list of leaves of the tree
treeLeaves :: Tree a -> [a]
treeLeaves Empty = []
treeLeaves (Node x []) = [x]
treeLeaves (Node x ts) = foldr (++) [] [treeLeaves t | t <- ts]

-- Return maximum element of the tree
treeMax :: (Ord a) => Tree a -> a
treeMax Empty       = error "Can't maximum on empty tree"
treeMax (Node x []) = x 
treeMax (Node x ts) = maximum [ treeMax t | t <- ts]

-- PreOrder traversal
preOrder :: Tree a -> [a]
preOrder Empty       = error "Tree is empty"
preOrder (Node x []) = [x]
preOrder (Node x ts) = x:(foldr (++) [] [preOrder t | t <- ts] )

-- Breadth-first tree traversal.
breadthFirst   :: Tree a -> [a]
breadthFirst x =  aux [x]
    where 
        aux [] =  []
        aux xs  =  map root xs ++ aux (concat (map children xs))
        root (Node x _)   =  x
        children (Node x ts) = ts


-- Demo Trees
demoTree :: Tree Int
demoTree = Node 1 [Node 2 [Node 12 [],
                          Node 10 []
                         ],
                  Node 3 [Node 9  []],
                  Node 7 [Node 8  [],
                          Node 21 []]
                  ]

demoBigTree :: Tree Int
demoBigTree = Node 20 [Node 10 [Node 5 [Node 1 [],
                                        Node 7 []
                                        ],
                                Node 15 []
                                ],
                       Node 30 [Node 25 [],
                                Node 35 [Node 32 [],
                                         Node 40 []
                                         ]
                                ]
                      ]









