-- 2. Define a polymorphic function for swapping components in a two components tuple:

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)
