-- 8. Let us consider the following definition to represent that one euro is 166.386 pesetas:

oneEuro :: Double
oneEuro = 166.386

-- a) Define a function named changing an amount of pesetas (expressed as a) into corresponding amount of euros. For instance:

pesetasToEuros :: Double -> Double 
pesetasToEuros x =x/oneEuro

--b) Define a function named changing euros into pesetas. For instance:

eurosToPesetas :: Double -> Double
eurosToPesetas x = x * oneEuro

-- c) Let us consider the following property, stating that by changing pesetas into euros and then changing back those euros into pesetas, we should obtain the original amount of pesetas (i.e., those are inverse functions): Test this property with QuickCheck and check that it does not hold. Why? (hint: these functions are defined on floating point numbers).

p_inverse x = eurosToPesetas (pesetasToEuros x) == x

-- Because some decimals are lost, it is not exactly equal, this will be fixed in the next exercise
