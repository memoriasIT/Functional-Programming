-- 6. Define an overloaded function for types with equality taking a tuple with three components of the same type and returning True if all of them are equal. For instance:

equals3 :: Eq a => (a, a, a) -> Bool
equals3 (x, y, z) = (x==y && y==z)
