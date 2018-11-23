
 ██████████                               
░░░░░██░░░                                
    ░██     ██████  █████   █████   ██████
    ░██    ░░██░░█ ██░░░██ ██░░░██ ██░░░░ 
    ░██     ░██ ░ ░███████░███████░░█████ 
    ░██     ░██   ░██░░░░ ░██░░░░  ░░░░░██
    ░██    ░███   ░░██████░░██████ ██████ 
    ░░     ░░░     ░░░░░░  ░░░░░░ ░░░░░░  


# Introduction

* Non linear datastrcture
* Hierarchical relationship
* Collection of elements
* Set of nodes
* Recursive definition

# Examples

* Maximal/minimal tree
* Syntax tree
* Binary Search Tree

# Terminology

            1
           root
        /        \
        2         3
 internal node   internal node
 /      \          /      \
 4      5          6      7
 leaf   leaf     leaf    leaf


 * Root: only one without parent (1)
 * Children: rest of elements that have parents
     * leafs: elements without children (4, 5, 6, 7)
     * internal nodes: not the root nor leafs (2 and 3)
 * Grandparent: parent of the parent (1 is grandparent of 4)
 * Siblings: children of the same parents (2 and 3 are siblings)
 * Height: number of levels of a tree, root is level 0

 
 * Proper Binary Tree
    - Every node has two children (leaves don't have to)


            1
        /        \
     2            3
 /      \          
 4      5          


* Complete Binary Tree
    - Every level is completely filled.
    - If not filled all nodes are as left as possible


            1
        /        \
     2            3
 /                 
 4                 

* Perfect Binary Tree
    - All leaves at the same level


            1
        /        \
     2            3
 /      \         |  \
 4      5         6   7



# Haskell 
data Tree a = Empty | Node a [Tree a] deriving Show

Implementation:
    tree1:: Tree Int
    tree1 =
        Node 1 [Node 2 [ Node 4 []
                       , Node 5 []
                       , Node 6 []
                       ]
               , Node 3 [ Node 7 [] ]
               ]

       / 4
       / 5
   / 2 - 6
1 <
   \ 3 - 7


            1
        /        \
     2            3
 /  |   \         |
 4  5   5         7


            
