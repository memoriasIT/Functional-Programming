{-


██╗  ██╗██╗   ██╗███████╗███████╗███╗   ███╗ █████╗ ███╗   ██╗
██║  ██║██║   ██║██╔════╝██╔════╝████╗ ████║██╔══██╗████╗  ██║
███████║██║   ██║█████╗  █████╗  ██╔████╔██║███████║██╔██╗ ██║
██╔══██║██║   ██║██╔══╝  ██╔══╝  ██║╚██╔╝██║██╔══██║██║╚██╗██║
██║  ██║╚██████╔╝██║     ██║     ██║ ╚═╝ ██║██║  ██║██║ ╚████║
╚═╝  ╚═╝ ╚═════╝ ╚═╝     ╚═╝     ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═══╝

* Huffman allows to represent texts in a compressed way.
* Length can vary: each character represented by a sequence of bits with a length following an inversely proportion by its aparition frequence.
* More frequent values → less bits

-}

import qualified AVLDictionary as D
import qualified WBLeftistHeapPriorityQueue as PQ
import Data.List(nub)


weights :: Ord a => [a] -> D.Dictionary a Int
weights x = peso x D.empty
{-

-- [ EXPECTED BEHAVIOUR ] --

 weights "abracadabra"
 AVLDictionary('a' → 5,'b'→ 2, 'c' → 1, 'd' → 1, 'r' → 2)

 weights [1,2,9,2,0,1,6,1,5,5,8]
 AVLDictionary(0→1, 1→3, 2→2, 5→2, 6→1, 8→1, 9→1)

 weights ""
 AVLDictionary()

-}

peso :: Ord a => [a] -> D.Dictionary a Int -> D.Dictionary a Int
peso [] d = d
peso (x:xs) d = peso xs (D.updateOrInsert x (+1) 1 d)

sacaJust :: Maybe a -> a
sacaJust Nothing = error "Nothing found"
sacaJust (Just x) x


