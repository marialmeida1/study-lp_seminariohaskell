-- Example 1: Making a custom type an instance of Eq
data Color = Red | Green | Blue deriving Show

instance Eq Color where
    Red == Red = True
    Green == Green = True
    Blue == Blue = True
    _ == _ = False

-- Example 2: Using a type class constraint in a function
compareColors :: Eq a => a -> a -> Bool
compareColors x y = x == y

-- Example 3: Creating a custom type class
class Describable a where
    describe :: a -> String

instance Describable Color where
    describe Red = "This is red."
    describe Green = "This is green."
    describe Blue = "This is blue."
