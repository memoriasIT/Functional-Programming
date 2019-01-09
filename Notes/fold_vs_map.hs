{-


   ████           ██      ██
  ░██░           ░██     ░██
 ██████  ██████  ░██     ░██
░░░██░  ██░░░░██ ░██  ██████
  ░██  ░██   ░██ ░██ ██░░░██
  ░██  ░██   ░██ ░██░██  ░██
  ░██  ░░██████  ███░░██████
  ░░    ░░░░░░  ░░░  ░░░░░░ 


        ██    ██  ██████
       ░██   ░██ ██░░░░ 
       ░░██ ░██ ░░█████ 
        ░░████   ░░░░░██
         ░░██    ██████ 
          ░░    ░░░░░░  


                       ██████ 
 ██████████   ██████  ░██░░░██
░░██░░██░░██ ░░░░░░██ ░██  ░██
 ░██ ░██ ░██  ███████ ░██████ 
 ░██ ░██ ░██ ██░░░░██ ░██░░░  
 ███ ░██ ░██░░████████░██     
░░░  ░░  ░░  ░░░░░░░░ ░░      




There is actually nothing in common, but for some reason I was misleading those lol.

--------------------------------------
--| FOLD

Type:
    foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b


GENERAL APPLICATION FOR FOLD:
    foldr f z [x:xs]

- f      → function to apply
- z      → first element (in general neutral element)
- [x:xs] → in general a list is provided


EXAMPLE:
    foldr (+) 0 [1, 2, 3] = 1 + ( 2 + ( 3 + 0 ) )
    
- f → sum
- z → starting from zero
- [1,.. 3] → list from 1 to 3


--------------------------------------
--| MAP

Type:  
    map :: (a → b) → [a] → [b]


GENERAL APPLICATION FOR MAP:
    map f [x:xs]

- f      → function to apply
- [x:xs] → elements to apply to

EXAMPLE:
    map even [1,2,3] → [False, True, False]

- f       → even
- [1,2,3] → elements to apply function to

  +-   /!\ HOWEVER!    ------------------------------------
  | Map can be implemented with fold
  |
  | map f xs = foldr (\x ys → f x:ys) [] xs
  |     → [f x1, f x2, f x3]
  |
  | In the end all we do is apply a lambda function where we concatenate the result of applying the function
  | The neutral/starting element is the empty list
  | xs is the list to apply the function to
  |
  | By contrast, fold is impossible to implement using map 
  |     foldr f z xs = map ? xs
  |
  +--------------------------------------------------------



-}





















