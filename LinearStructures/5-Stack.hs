------------------------------------------------------------------------
-- 5. [DONE] Use a Stack for checking if chars are balanced           --
------------------------------------------------------------------------

{-
TODO: Implement check function


Desired output:
    vv(hg(jij)hags{ss[dd]dd})  → True
      ^  ^   ¡    ^  ^  ¡  ¡¡
      |  |   |    |  |  |  ||
      |  |   |    |  +--+  ||
      |  +---+    +--------+|
      +---------------------+

    ff(h([sds)sds]ss)hags      → False
      ^ ^^   ¡   ¡  ¡ 
      | @|   @   |  |
      |  +-------+  |
      +-------------+

   
   ALG0R1THM:
    * Should ignore the rest of chars
    * Process from left to right
    * Push "(" "[" "{" to Stack
    * Pop ")" "]" "}" from Stack
    * Will be balanced if all operations match and stack is empty
-}

data Stack a = Empty | Node a (Stack a)

empty :: Stack a
empty = Empty

isEmpty :: Stack a -> Bool
isEmpty Empty = True
isEmpty _     = False

push :: a -> Stack a -> Stack a
push x s = Node x s

top :: Stack a -> a
top Empty      = error "Can't do top on empty Stack"
top (Node x s) = x

pop :: Stack a -> Stack a
pop Empty      = error "Can't pop on empty stack" 
pop (Node x s) = s


checkString :: [Char] -> Bool
checkString "" = True
checkString (x:xs) = check (x:xs) Empty  
    where 
      check :: [Char] -> Stack Char -> Bool
      check [s] sc = True
      check (x:xs) sc
        | (elem x "([{") && xs == []                      = False
        | elem x "([{"                                    = check xs (push x sc) 
        | (elem x ")]}") && (top sc) == (inversechar x)   = check xs (pop sc)
        | elem x ")]}"                                    = False
        | otherwise                                       = check xs sc
      inversechar ']' = '['
      inversechar ')' = '('
      inversechar '}' = '}'







