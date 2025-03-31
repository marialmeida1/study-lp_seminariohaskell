-- Example 1: A pure function that always produces the same output for the same input
square :: Int -> Int
square x = x * x

-- Example 2: A pure function that combines two inputs
add :: Int -> Int -> Int
add x y = x + y

-- Example 3: A pure function using recursion
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
