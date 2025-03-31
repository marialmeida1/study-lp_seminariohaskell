#  Tutorial de Introdução à Linguagem Haskell

##  1. Instalação

### Usando o GHCup (recomendado)
O **GHCup** é a forma mais prática de instalar Haskell e ferramentas relacionadas.

#### Para Linux/macOS:
```bash
curl -sSL https://get-ghcup.haskell.org | bash
```

#### Para Windows:
Baixe o instalador em: [https://www.haskell.org/ghcup/](https://www.haskell.org/ghcup/)

Esse processo instalará:
- GHC (compilador Haskell)
- Cabal (gerenciador de pacotes/projetos)
- HLS (Haskell Language Server)
- Stack (alternativa ao cabal)

Verifique se tudo está funcionando:
```bash
ghc --version
cabal --version
```

## 🛠️ 2. Compilação por linha de comando

### Interpretar um arquivo `.hs` (modo interativo):
Utilize o GHCi (interpretador interativo) para testar funções rapidamente sem compilar:
```bash
ghci exemplo.hs
```

### Compilar para um executável:
Para gerar um executável a partir do seu código:
```bash
ghc exemplo.hs -o exemplo
./exemplo
```

##  3. Estrutura básica do código Haskell

A estrutura mínima de um programa em Haskell é composta pelo módulo principal e uma função `main`:

```haskell
-- Comentário de linha
{-
  Comentário de múltiplas linhas
-}

-- Módulo principal
main :: IO ()
main = putStrLn "Olá, mundo!"
```

Neste exemplo, `putStrLn` imprime uma string no terminal. A anotação `:: IO ()` indica que a função realiza uma ação de entrada/saída.

##  4. Conceitos básicos da linguagem e sintaxe

###  Declaração de variáveis
Em Haskell, variáveis são imutáveis e geralmente chamadas de "bindings":

```haskell
nome = "Miguel"
idade = 21
```

###  Tipagem explícita
Você pode (e deve) declarar o tipo da variável ou função usando `::`:

```haskell
nome :: String
nome = "Miguel"

idade :: Int
idade = 21
```

###  Sintaxe de funções
Funções são declaradas de forma simples:

```haskell
soma :: Int -> Int -> Int
soma a b = a + b
```

A função `soma` recebe dois inteiros e retorna um inteiro. Os parâmetros são separados por espaço, não por vírgula.

###  Condicionais
Você pode usar `if`, `then`, `else` ou guardas:

```haskell
maioridade :: Int -> String
maioridade idade = if idade >= 18 then "Adulto" else "Menor"
```

###  Guardas
Uma forma mais legível de escrever condicionais:

```haskell
classificacao :: Int -> String
classificacao nota
  | nota >= 90 = "A"
  | nota >= 80 = "B"
  | otherwise  = "C ou inferior"
```

### ✳ Tuplas
Agrupam múltiplos valores com diferentes tipos:

```haskell
ponto :: (Int, Int)
ponto = (3, 4)
```

###  Listas
Listas são coleções homogêneas de elementos:

```haskell
numeros :: [Int]
numeros = [1, 2, 3, 4, 5]
```

##  5. Funcionalidades marcantes com exemplos

###  Funções puras e recursão
Haskell valoriza funções puras, que sempre produzem o mesmo resultado para os mesmos argumentos.

```haskell
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)
```

###  Pattern Matching
Pattern Matching facilita a definição de diferentes comportamentos para diferentes padrões de entrada.

```haskell
parOuImpar :: Int -> String
parOuImpar n
  | even n    = "Par"
  | otherwise = "Ímpar"
```

###  Funções de alta ordem
Haskell trata funções como cidadãos de primeira classe. É possível passá-las como argumento.

```haskell
aplicaDuasVezes :: (a -> a) -> a -> a
aplicaDuasVezes f x = f (f x)
```

###  Map, Filter, Lambda
Essas funções permitem aplicar operações sobre listas de forma elegante e concisa.

```haskell
quadrados = map (\x -> x * x) [1..5]
pares = filter even [1..10]
```

###  List Comprehension
Permite gerar listas com base em regras declarativas.

```haskell
dobros = [2*x | x <- [1..5]]
```

###  Tipos personalizados (Algebraic Data Types)
Você pode criar seus próprios tipos e trabalhar com eles de forma segura e clara.

```haskell
data Cor = Vermelho | Verde | Azul deriving Show

corFavorita :: Cor -> String
corFavorita Vermelho = "Você gosta de paixão!"
corFavorita Verde    = "Você ama natureza!"
corFavorita Azul     = "Você é calmo."
```

##  6. Testando no GHCi

Abra o GHCi para testar suas funções rapidamente:

```bash
ghci
> fatorial 5
> aplicaDuasVezes (*2) 3
> quadrados
```

