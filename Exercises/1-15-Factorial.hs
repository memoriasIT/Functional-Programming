--15. For a set whose elements are all different, a permutation of it is each possible rearrangement of its elements. For instance, for set {1,2,3}, there exists 6 different permutations of its elements: {1,2,3}, {1,3,2}, {2,1,3}, {2,3,1}, {3,1,2} y {3,2,1}. For a set with n elements, the number of permutations is the factorial of n (usually written as n!), and defined as the product of natural numbers from 1 to n. Define a function taking a natural number and returning its factorial. As factorial function grows very quickly, use type in order to avoid overflows. For instance:

factorial :: Integer -> Integer
factorial 1 = 1
factorial x = x * factorial (x-1)


