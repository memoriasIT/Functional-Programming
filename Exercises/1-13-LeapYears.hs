-- 13. Leap years are those which are multiple of 4. One exception to the rule are those years that are multiples of 100, which are only considered leap if they are additionally multiple of 400. Define a function named taking a year and returning if it is a leap year. For example:

--Hint: use the logical implication operator defined in previous exercise in order to represent the following sentence: “n is a leap year if it fulfills the following two conditions: (a) it is a multiple of 4, and (b) if n is a multiple of 100 then n should be a multiple of 400”.

-- From last exercises
infix 4 ==>>
(==>>) :: Bool -> Bool -> Bool
True  ==>> y = y
False ==>> y = True 

isMultiple :: Integer -> Integer -> Bool
isMultiple x y = mod x y == 0



leapYear :: Integer -> Bool
leapYear x = isMultiple x 4 && isMultiple x 100 ==>> isMultiple x 400

