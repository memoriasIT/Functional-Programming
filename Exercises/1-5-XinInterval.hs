--  5. Define an overloaded function for ordered types taking a value x and a tuple with two components (max,min) and testing whether x is in the interval determined by min and max, i.e., if x ∈ [min,max]. For instance:

between :: Ord a => a -> (a,a) -> Bool
between x (y, z) = x >= y && x <= z
