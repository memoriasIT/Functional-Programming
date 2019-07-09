import Test.QuickCheck

-- 1. Three positive integer values (x, y, z) are a Phytagoran triple if x²+y²=z², that is to say, if they correspond to lengths for sides of a triangle.
-- a) Define a function to test whether three values are a Phytagorean triple. For instance:

isTriple :: Integer -> Integer -> Integer -> Bool
isTriple x y z = (x^2) + (y^2) == (z^2)

-- b) For any x and y positive integers such that x>y, triple (x2-y2, 2xy, x2+y2) is Phytagorean. Accordingly,write a function named taking two numbers and returning a Phytagorean triple. For instance:

triple :: Integer -> Integer -> (Integer, Integer, Integer)
triple x y 
    | x < y = error "X must be greater than Y"
    | otherwise = (x^2 - y^2, 2*x*y, x^2 + y^2)

-- c) Read and understand the following property, that states that all triples generated by function are Phytagorean:

p_triples x y = x>0 && y > 0 && x>y ==> isTriple l1 l2 h
 where
     (l1, l2, h) = triple x y


-- d) Check this property using QuickCheck (recall to import at the beginning of your program and to copy the property into your file). You should observe a result similar to the following one:

-- quickCheck p_triples

-- *Main> quickCheck p_triples
-- +++ OK, passed 100 tests; 849 discarded.

-- if not found --> cabal install QuickCheck
-- <no location info>: error:
--     Could not find module ‘Test.QuickCheck’
--     It is not a module in the current program, or in any known package.

