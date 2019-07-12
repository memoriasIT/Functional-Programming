-- 12. Define a left associative logical implication operator so that its precedence is lower than the one corresponding to conjunction and disjunction operators:
-- 3 < 1 ==>> 4 > 2
--Hint: you can directly define it using a single equation:
--Or you can define it by using multiple equations and patterns:

--(==>>) :: Bool -> Bool -> Bool
-- (p→q)→r (left associativity???)
infix 4 ==>> 
(==>>) :: Bool -> Bool -> Bool
True  ==>> y = y
False ==>> y = True 
