--17. The median of a set of numbers is the value for which half the numbers are larger and half are smaller. Define a function to compute the median for a tuple with 5 components so that, for instance, Observe that 1,3 ≤ 10 ≤ 20,50. According to this observation, define this function using guards by filling in the following outline:


median :: Ord a => (a, a, a, a, a) -> a
median (x, y, z, t, u)
    | x > z              = median (z, y, x, t, u)
    | y > z              = median (x, z, y, t, u)
    | u < z              = median (x, y, u, t, z)
    | t < z              = median (x, y, t, z, u)
    | otherwise          = z 

