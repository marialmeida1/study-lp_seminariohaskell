# Exemplos de Código Haskell com Explicações

## 1. Olá, Mundo

```haskell
main :: IO ()
main = putStrLn "Olá, mundo!"
```

### Explicação:
- `main :: IO ()`: define o tipo da função `main` como uma ação de entrada/saída que não retorna valor significativo.
- `putStrLn "Olá, mundo!"`: imprime a mensagem no terminal.

---

## 2. Soma de Dois Números

```haskell
soma :: Int -> Int -> Int
soma a b = a + b

main = print (soma 3 4)
```

### Explicação:
- `soma :: Int -> Int -> Int`: recebe dois inteiros e retorna um inteiro.
- `soma a b = a + b`: corpo da função que realiza a soma.
- `main = print (soma 3 4)`: imprime o resultado da função com os argumentos 3 e 4 (resultado: 7).

---

## 3. Fatorial Recursivo

```haskell
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

main = print (fatorial 5)
```

### Explicação:
- `fatorial :: Integer -> Integer`: declara o tipo da função.
- `fatorial 0 = 1`: caso base da recursão.
- `fatorial n = n * fatorial (n - 1)`: chamada recursiva.
- Resultado impresso será `120`.

---

## 4. Par ou Ímpar com Guardas

```haskell
parOuImpar :: Int -> String
parOuImpar n
  | even n    = "Par"
  | otherwise = "Ímpar"

main = putStrLn (parOuImpar 7)
```

### Explicação:
- `even n`: retorna `True` se o número for par.
- `otherwise`: pega os casos não cobertos (como um `else`).
- O código imprime "Ímpar", já que 7 não é par.

---

## 5. Lista de Quadrados com Lambda

```haskell
quadrados = map (\x -> x * x) [1..5]

main = print quadrados
```

### Explicação:
- `map` aplica uma função a cada elemento da lista.
- `\x -> x * x`: função lambda (anônima) que retorna o quadrado do número.
- Resultado: `[1,4,9,16,25]`.
