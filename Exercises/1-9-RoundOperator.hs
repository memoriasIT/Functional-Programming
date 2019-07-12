import Test.QuickCheck

-- 9. Let us consider the following operator to test whether two values are approximately equal: For instance: Copy this operator definition into your file, and restate property p_inverse so that it holds. Test that your new definition for the property does indeed hold using QuickCheck.

infix 4 ~=
(~=) :: Double -> Double -> Bool
x ~= y = abs (x-y) < epsilon
    where epsilon = 1/1000



-- From last exercise
oneEuro :: Double
oneEuro = 166.386

pesetasToEuros :: Double -> Double 
pesetasToEuros x =x/oneEuro

eurosToPesetas :: Double -> Double
eurosToPesetas x = x * oneEuro

-- QuickCheck Test
p_inverse' x = eurosToPesetas (pesetasToEuros x) ~= x
