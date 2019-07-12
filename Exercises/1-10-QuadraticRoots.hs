import Test.QuickCheck

-- 10. Let us consider the quadratic equation ax2 + bx + c = 0.
-- a) Define a function named roots taking three arguments (corresponding to equation coefficients a, b and c) and returning a tuple with two real solutions to the equation (you can use predefined function in order to compute the square root of a floating number). Recall that the discriminant is defined as b2-4ac and that a quadratic equation has real solutions if the corresponding discriminant is non-negative. For instance:

-- roots 1 (-2) 1.0 => (1.0, 1.0)
-- roots 1.0 2 4 => Exception: Non real roots

-- ax² + bx + c = 0
roots :: Double -> Double -> Double -> (Double, Double)
roots a b c
    | sqrteq >= 0   = ((-b + sqrt sqrteq )/ (2*a), (-b - sqrt sqrteq) / (2*a))
    | otherwise     = error "Roots are not real"
    where sqrteq = b^2 - 4*a*c

-- b) Let us consider the following property to test that values returned by
--equation:

-- From last exercise: 
infix 4 ~=
(~=) :: Double -> Double -> Bool
x ~= y = abs (x-y) < epsilon
    where epsilon = 1/1000

p1_roots a b c = isRoot r1 && isRoot r2
    where
        (r1, r2) = roots a b c
        isRoot r = a * r^2 + b*r + c ~= 0


--Test this property using QuickCheck and check that it fails. Find out the reason, and add appropriate preconditions so that it does not fail by filling in interrogations in the following outline:

p2_roots a b c = ((((b)^2 - 4*(a)*(c)) >= 0) && (a /= 0)) ==> isRoot r1 && isRoot r2
    where 
        (r1, r2) = roots a b c
        isRoot r = a*r^2 + b*r +c ~= 0

--so that:
-- quickCheck p2_roots
-- +++ Ok, passed 100 tests

