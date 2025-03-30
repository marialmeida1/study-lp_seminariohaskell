-- Example 1: Infinite list of numbers
infiniteNumbers :: [Int]
infiniteNumbers = [1..] -- Infinite list

-- Example 2: Taking the first 5 numbers from an infinite list
firstFive :: [Int]
firstFive = take 5 infiniteNumbers -- [1, 2, 3, 4, 5]

-- Example 3: Lazy evaluation with a custom infinite sequence
fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

firstTenFibonacci :: [Integer]
firstTenFibonacci = take 10 fibonacci -- [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
