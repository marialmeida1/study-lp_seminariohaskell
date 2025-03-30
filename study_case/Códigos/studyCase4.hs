-- Exemplo de programa interativo
programaIO :: IO ()
programaIO = do
    putStrLn "Qual é o seu nome?"
    nome <- getLine
    putStrLn $ "Olá, " ++ nome ++ "!"
    putStrLn "Digite dois números para somar:"
    num1 <- readLn
    num2 <- readLn
    putStrLn $ "Resultado: " ++ show (num1 + num2)