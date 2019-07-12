-- 14. Although there already exists a predefined operator ( ) in Haskell to compute raising of a number to a power, the aim of this exercise is that you define your own version of this operator. 
-- a) By using the following property bn = b ∙ bn-1 define a recursive function taking an integer b and a natural exponent n and returning bn. Forexample:
-- power 2 3 => 8

power :: Integer -> Integer -> Integer
power x 0 = 1
power x y | y>0 = x * power x (y-1)
power x y = error "Negative Exponent" 

--b) Let us now consider the following property:
--𝑏^𝑛 = { (𝑏^𝑛/2)²       if 𝑛 is even }
--      {   𝑏 ˑ 𝑏^𝑛−1     if 𝑛 is odd }

--Turn this property into a new recursive definition, and accordingly, define a recursive
--function taking an integer b and a natural exponent n and returning bn (don’t use previous
--function in your new definition). For example:

-- power' 2 3 => 8          power' 2 4 => 16

power' :: Integer -> Integer -> Integer
power' b n
    |  mod n 2 == 0     = ((b)^(div n 2))^2
    |  otherwise        = b * power' b (n-1) 

-- c) Test using QuickCheck correctness of both functions by means of the following property:

p_power b n = n >= 0 ==> power b n == sol && power' b n == sol
     where sol = b ^ n

--d) Assuming that doing a square operation corresponds to a doing product, determine the number of products that both functions do in order to raise some base to an exponent n.
--Hint: in order to analyze efficiency for function, consider first exponents that are power of 2.

-- 2³ = power 2 (power 2 (power 2 (1))) 
-- 2⁵ = 2 * 2 * 2 * 2 * 2

-- It will be O(n) efficient
