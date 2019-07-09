import Test.QuickCheck

-- 7. Recall that the quotient and the modulo for an integer division can be calculated by using predefined functions div and mod

-- a) Define a function named decompose taking an integer value representing seconds, and returning a three components tuple with corresponding hours, minutes and seconds, so that returned minutes and seconds are in the range 0 to 59. For instance:
-- Define this function by filling in the following outline:
type Hour = Integer
type Minute = Integer
type Second = Integer
decompose :: Second -> (Hour, Minute, Second)
decompose x = (hours, minutes, seconds)
    where 
        hours = mod (div x 3600) 60 
        minutes = mod (div x 60) 60 
        seconds = mod x 60

-- b) Read and test the following property in order to check correctness for your function:
-- (Between from the exercise before)
between :: Ord a => a -> (a,a) -> Bool
between x (y, z) = (x >= y && x <= z)

p_decompose x = x >= 0 ==> h*3600 + m*60 + s == x
                             && between m (0,59)
                             && between s (0, 59)
     where (h, m, s) = decompose x
