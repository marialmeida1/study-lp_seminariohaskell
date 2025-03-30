parOuImpar :: Int -> String
parOuImpar n
  | even n    = "Par"
  | otherwise = "Ímpar"

main = putStrLn (parOuImpar 7)
