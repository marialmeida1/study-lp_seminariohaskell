# Estudo de caso
Esta documentação apresenta quatro estudos de caso que destacam as principais características da linguagem Haskell, contrastando-as com abordagens imperativas. Os exemplos foram selecionados para demonstrar:
1. Avaliação preguiçosa com listas infinitas (Sequência de Fibonacci)
2. Expressividade e concisão (Implementação do Quicksort)
3. Funções como valores de primeira classe
4. Gerenciamento de efeitos colaterais com monads

## Estudo de Caso 1: Avaliação Preguiçosa e Listas Infinitas (Fibonacci)
**Objetivo**: Demonstrar como a avaliação preguiçosa permite trabalhar com estruturas de dados potencialmente infinitas de maneira eficiente.


```haskell
-- Definição da sequência de Fibonacci como lista infinita
fibos :: [Integer]
fibos = 0 : 1 : zipWith (+) fibos (tail fibos)

-- Uso: pega os primeiros 20 números
main :: IO ()
main = print $ take 20 fibos
 ```  

#### Características destacadas:
1. **Avaliação preguiçosa**: Haskell só calcula os elementos quando necessários 
2. **Definição declarativa**: A sequência é definida em termos de si mesma
3. **Imutabilidade**: fibos é uma constante infinita

### Comparação com Python:
``` python
def fibonacci(n):
    """Retorna os primeiros n números da sequência de Fibonacci."""
    fib = [0, 1]
    for _ in range(n - 2):
        fib.append(fib[-1] + fib[-2])
    return fib

if __name__ == "__main__":
    print(fibonacci(20))
 ```  
#### Análise comparativa:
- Em Python precisamos de um gerador e loop explícito
- Haskell expressa o conceito matemático diretamente
- A versão Haskell não precisa de instruções de controle (for ou while)

## Estudo de Caso 2: Concisão e Expressividade (Quicksort)
**Objetivo**: Mostrar como Haskell permite traduzir diretamente conceitos matemáticos em código executável.

```haskell
-- Recebe uma lista de 'a' e retorna uma lista ordenada de 'a'
quicksort :: Ord a => [a] -> [a]
quicksort []     = [] -- Caso base: se a lista for vazia, retorna lista vazia
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater) -- Caso recursivo: para uma lista com primeiro elemento 'p' e resto 'xs'
    where
        lesser  = filter (< p) xs -- ' lista com todos elementos de 'xs' menores que pivo
        greater = filter (>= p) xs  -- ' lista com todos elementos de 'xs' maiores ou iguais ao pivo
 ```  
#### Características destacadas:
1. **Pattern matching**: Casamento de padrões para decompor a lista
2. **Imutabilidade**: Nenhum elemento é modificado, novas listas são criadas
3. **Recursão**: Algoritmo expresso naturalmente via recursão
4. **Polimorfismo**: Funciona para qualquer tipo ordenável (Ord a =>)

### Comparação com Java:
```Java
public class studyCase2 {
    /**
	 * Algoritmo de ordenacao Quicksort.
     * @param int[] array vetor a ser ordenado
     * @param int esq inicio do array a ser ordenado
     * @param int dir fim do array a ser ordenado
	 */
    private void quicksort(int[] array, int esq, int dir) {
        int i = esq, j = dir;
        int pivo = array[(dir+esq)/2];
        while (i <= j) {
            while (array[i] < pivo) i++;
            while (array[j] > pivo) j--;
            if (i <= j) {
                swap(array, i, j);
                i++;
                j--;
            }
        }
        if (esq < j)  quicksort(array,esq, j);
        if (i < dir)  quicksort(array,i, dir);
    }
    
}

 ```  
#### Análise comparativa:
- Java requer gerenciamento explícito de memória (Array)
- Haskell usa filtros e concatenação de listas de forma declarativa
- Versão Java tem 15 linhas vs 6 em Haskell
- Java precisa de tipos concretos (vetor de int) enquanto Haskell é polimórfico

## Estudo de Caso 3: Funções de Primeira Classe
**Objetivo**: Demonstrar como funções podem ser tratadas como valores comuns em Haskell.

```haskell
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
 ```  

#### Características destacadas:
1. **Funções como parâmetros**: integral recebe uma função f
2. **Funções como retorno**: multiplicador retorna uma função lambda
3. **Composição**: Operador . combina funções

#### Vantagens:
- Código mais modular e reutilizável
- Facilidade para programação funcional (map, filter, fold)
- Permite programação funcional avançada (Currying e Composição)
- Facilidade para Paralelismo e Lazy Evaluation

## Estudo de Caso 4: Monads para Efeitos Colaterais
**Objetivo**: Mostrar como Haskell lida com efeitos colaterais de maneira controlada e tipada.

```haskell
programaIO :: IO ()
programaIO = do
    putStrLn "Qual é o seu nome?"
    nome <- getLine
    putStrLn $ "Olá, " ++ nome ++ "!"
    putStrLn "Digite dois números para somar:"
    num1 <- readLn
    num2 <- readLn
    putStrLn $ "Resultado: " ++ show (num1 + num2)
 ```  

#### Características destacadas:
1. **Separação clara**: Código puro vs. código com efeitos colaterais
2. **Sequenciamento explícito**: Operador <- para ações sequenciais
3. **Segurança**: Tipo IO marca computações impuras
4. **Composição**: do notation permite encadear ações

## Conclusão
Os estudos de caso demonstram como Haskell aborda problemas comuns de programação de maneira distinta das linguagens imperativas:
 1. Listas infinitas: Avaliação preguiçosa permite estruturas infinitas e melhor modularidade
 2. Algoritmos concisos: Implementações mais próximas das definições matemáticas
 3. Funções como cidadãs de primeira classe: Maior poder de abstração e composição
 4. Efeitos controlados: Sistema de tipos rígido para operações impuras

Juntas, estas características permitem desenvolver software mais robusto, modular e matematicamente correto, com menos código, menos erros e maior facilidade de manutenção. 

