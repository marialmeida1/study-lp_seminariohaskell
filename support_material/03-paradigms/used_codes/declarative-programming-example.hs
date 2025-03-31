-- Example 1: Filtering even numbers from a list
evenNumbers :: [Int]
evenNumbers = filter even [1..10] -- [2, 4, 6, 8, 10]

-- Example 2: Combining map and filter
squaredEvenNumbers :: [Int]
squaredEvenNumbers = map (^2) (filter even [1..10]) -- [4, 16, 36, 64, 100]

-- Example 3: Using fold to sum squares of even numbers
sumOfSquares :: Int
sumOfSquares = foldl (+) 0 squaredEvenNumbers -- 220
