-- Example 1: Variables are immutable
x :: Int
x = 5
-- x = 6 -- Uncommenting this line will cause a compilation error

-- Example 2: Immutable data structures
originalList :: [Int]
originalList = [1, 2, 3]

newList :: [Int]
newList = 0 : originalList -- Creates a new list [0, 1, 2, 3]

-- Example 3: Transforming a list without modifying the original
incrementedList :: [Int]
incrementedList = map (+1) originalList -- [2, 3, 4]
