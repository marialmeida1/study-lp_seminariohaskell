-- Declaração do tipo da função quicksort:
-- 'Ord a =>' significa que o tipo 'a' deve ser ordenável (permitir comparações)
-- Recebe uma lista de 'a' e retorna uma lista ordenada de 'a'
quicksort :: Ord a => [a] -> [a]
quicksort []     = [] -- Caso base: se a lista for vazia, retorna lista vazia
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater) -- Caso recursivo: para uma lista com primeiro elemento 'p' e resto 'xs'
    where
        lesser  = filter (< p) xs -- 'lesser' é uma lista com todos elementos de 'xs' menores que 'p' (o pivô)
        greater = filter (>= p) xs  -- 'greater' é uma lista com todos elementos de 'xs' maiores ou iguais a 'p'