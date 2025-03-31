-- Função que recebe outra função como parâmetro
integral :: (Double -> Double) -> Double -> Double -> Double
integral f a b = sum [f x * delta | x <- [a, a+delta .. b]]
    where delta = 0.0001  -- Precisão do cálculo

-- Função que retorna uma função
multiplicador :: Double -> (Double -> Double)
multiplicador factor = \x -> x * factor

-- Composição de funções
transformacao :: Double -> Double
transformacao = sin . multiplicador 2 . cos  -- sin(2 * cos(x))

-- Uso:
main :: IO ()
main = do
    -- 1. Passando função como argumento
    putStrLn $ "Integral de x²: " ++ show (integral (^2) 0 1)
    
    -- 2. Armazenando função em variável
    let dobro = multiplicador 2
    putStrLn $ "Dobro de 5: " ++ show (dobro 5)
    
    -- 3. Composição de funções
    putStrLn $ "Transformação de π: " ++ show (transformacao pi)