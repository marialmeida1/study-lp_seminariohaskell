# Principais características programáticas:

- ## Programação puramente funcional:
  * ### Idéia principal: 
    O principal ponto da programação puramente funcional, é que ele trata a computação como a avaliação de funções matemáticas. 
  * ### Recursos chave neste tipo de programação:
   1. #### Imutabilidade:
      Dados não podem ser alterados após serem criados.
      ##### Como a linguagem enforça a imutabilidade:
       - Variáveis: Em haskell, as variáveis são na verdade constantes. Quando se associa um valor à uma variável, ele não pode ser alterado. 
       ```haskell
       x = 5
       x = 6 -- This would cause an error
       ```  
       - Estruturas de dados: Listas, tuplas e outras estruturas de dados em Haskell são imutáveis. Se você quiser "modificar" uma lista, você pode criar uma nova lista ao invés disso.
       ```haskell
       let list = [1, 2, 3]
       let newList = 0 : list
       -- Cria uma nova lista [0,1,2,3]
       ```
      ##### Benefícios da imutabilidade em Haskell:
       - Sem efeitos colaterais: As funções em Haskell não podem alterar dados externos, assegurando, assim, que são puras e predisíveis
       - Fácil de debuggar: Já que dados não mudam, você não precisa rastrear como e onde ele foi modificado
       - Composição funcional: A imutabilidade torna mais fácil de se combinar funções menores em funções maiores sem se preocupar sobre mudanças não desejadas dos dados.
     
      ##### Exemplo de imutabilidade em ação:
      Suponha que você tem uma lista de números e você deseja adicionar 1 a cada número. Ao invés de modificar a lista original, você cria uma nova em seu lugar:
      ```haskell
      let numbers = [1, 2, 3]
      let incremented = map (+1) numbers --Agora a nova lista 'incremented' será [2, 3, 4]
      -- A lista original 'numbers' continua semm mudanças
      ```
      > A função <code>map</code> em Haskell é utilizada para se iterar uma função (que é passada como argumento da própria função map) à cada elemento de uma lista, produzindo uma nova lista com os resultados (outputs) da iteração desta função sobre cada elemento da lista inicial.
      > Sintaxe:
      > ```haskell
      >    map:: (a->b) -> [a] -> [b]
      > ```
      >  - <code>(a -> b)</code>: A função que ao receber o elemento <code>a</code> retorna o valor do tipo <code>b</code>.
      >  - <code>[a]</code>: A lista de entrada (input da função) contendo elementos de tipo <code>a</code>.
      >  - <code>[b]</code>: A lista de saída (output da função) contendo elementos do tipo <code>b</code>.  
     

       
   2. #### Funções puras:
      Funções sempre produzem o mesmo "output" para o mesmo "input" sem produzir efeitos colaterais <code>Não modifica variáveis globais e nem printa para a tela</code>.

   3. #### Funções 'First-Class' ou 'De primeira classe':
      Em Haskell, as funções são tratadas como valores, portanto significando que podem ser passadas como argumentos (assim como pudemos ver na função map, onde outras funções são passadas como argumento na sua sintaxe), também podem ser retornadas de outras funções, ou ainda ser armazenadas em variáveis.
   

- ## Avaliação “preguiçosa”: 
  Em Haskell, os valores só são computados quando são necessários, além de ser um recurso chave e que garante a identidade da linguagem, têm implicações severas para como os programas são escritos e executados.

  * ### Como a avaliação preguiçosa funciona:
    Na maioria das linguagens de programação, quando você escreve uma expressão, <code>Ex: 2 + 3</code> ela é avaliada imediatamente, e o resultado é armazenado.
    Em Haskell, a expressão não é avaliada logo de cara. Ao invés disso, ela é representada como um "thunk".
    <code>Um thunk é uma estrutura de dados que representa uma computação que ainda não foi realizada. Ele armazena a expressão e as informações necessárias para avaliá-la no futuro.  </code>
    A computação das operações e a avaliação dos valores só ocorre quando o programa necessita explicitamente do resultado.

  * ### Benefícios:
    1) #### Eficiência:
       Haskell evita computações e uso de recursos computacionais desnecessários. Por exemplo, se você escreve uma função que geraria uma longa lista mas apenas utilizaria alguns elementos, Haskell irá computar apenas estes elementos e ignorar o resto.

    2) #### Estrutura de dados infinita:
       A avaliação preguiçosa permite que você trabalhe com listas ou estruturas de dados infinitas. Por exemplo:

       ```haskell
       let numbers = [1..] -- Lista infinita de números
       take 5 numbers      -- Só computa os primeiros 5 números
                           -- Resultado: [1,2,3,4,5]
       ```

    3) #### Modularidade:
       Você pode separar a geração de dados de seu consumo. Isto torna mais fácil escrever códigos reutilizáveis e componíveis.

  * ### Desafios do uso de avaliação preguiçosa:
     - Uso de memória: Se você não tomar cuidado, a avaliação preguiçosa pode levar a problemas de memória. Por exemplo, se um programa builda thunks não avaliados demais, isso pode consumir um monte de memória (isso é chamado de "space leak").
     - Debugging: Como as computações são derreferenciadas, as vezes pode ser ainda mais difícil traçar o fluxo de execução ou entender gargalos de performance.

  * ### Outras considerações sobre a avaliação preguiçosa:
    A maioria das linguagens de programação <code>Python, Java</code> utiliza avaliação estria, onde as expressões são computadas e avaliadas assim que encontradas, diferentemente do Haskell.
    A avaliação preguiçosa é particularmente útil quando:
      - Você está trabalhando com um conjunto extenso ou infinito de dados.
      - Você quer separar a geração de dados de seu consumo.
      - Você quer melhorar a performance ao evitar computações desnecessárias.
<br>

- ## Tipagem estática:
   #### Idéia principal: Tipos são checados em tempo de compilação, e não em tempo de execução.
    * ### Recursos chave:
     1. #### Tipagem forte:
          A tipagem sendo implementada desta forma em Haskell, previne a mistura de tipos incompatíveis <code>Adição de um tipo inteiro à uma String por exemplo</code>.
     2. #### Inferência de tipos:
          Haskell pode sempre descobrir o tipo da variável ou função sem você explicitamente especificá-la.
          Como a inferência de tipos funciona:
          - ##### Haskell analisa o código:
            O compilador analisa as operações e funções que são aplicadas à variável ou expressão, e baseado nisso, ele deduz o tipo mais específico que satisfaz todas as restrições
          - ##### Não há necessidade de anotações explícitas de tipo:
            Enquanto você pode explicitamente especificar os tipos para tornar uma documentação mais clara e legível, Haskell não precisa disso na maioria dos casos.

          -  ### Exemplos:
          > - ### Inferindo o tipo de uma variável:
          >    ```haskell
          >    x = 42 
          >    ```
          >    - O valor 42 é armazenado em X.
          >    - Como 42 é um inteiro, Haskell infere que x tem o tipo Num a => a (um tipo numérico)
          >   
          > - ### Inferindo o tipo de uma função:
          >   ```haskell
          >   add a b = a + b
          >   ```
          >    - A função add recebe dois argumentos (a b) e os adiciona.
          >    - Haskell infere que a função add funciona com tipos numéricos, portanto o tipo é:
          >      1. <code>add :: Num a => a -> a -> a</code>.
          >      2. <code>Num a =></code> é uma restrição que diz que <code>a</code> deve pertencer ao '[type-class](https://github.com/marialmeida1/study-lp_seminariohaskell/edit/master/support_material/03-paradigms/FEATURES.md#o-que-%C3%A9-type-class-em-haskell)' <code>Num</code>. Portanto deve ser um tipo numérico.
          >      3. <code>a -> a -> a</code> Descreve os inputs e outputs da função, querendo dizer que:
          >      4. A função recebe dois argumentos de tipo <code>a</code> <code>a -> a</code>.
          >      5. Ela retorna um valor de tipo <code>a</code>.
          >      6. Portanto, <code>add</code> recebe dois números de mesmo tipo e retorna um número de mesmo tipo.

  3. #### O que é uma type-class em Haskell:
     Em Haskell, um *type-class* ou classe de tipos, é uma maneira de se definir um grupo de funções ou operações que podem ser aplicados a diferentes tipos. É similar a uma interface em outras linguagens de programação. Um type-class especifica um comportamento, e tipos que implementam este comportamento são ditos como instâncias deste type-class.

     - #### Conceitos chave de type-classes:
         1. ##### Definindo uma type-class:
            * Uma type-class define um conjunto de funções que devem ser implementadas por qualquer tipo que quiser ser uma instância desta 'classe' (não confunda de forma alguma com as classes convencionais encontradas na programação orientada a objetos).
            * Exemplo:
              ```haskell
              class Eq a where
                  (==) :: a -> a -> Bool
                  (/=) :: a -> a -> Bool
              ```
              * `Eq` é um type-class para tipos que suportam comparações de igualdade (`==` e `/=`).
              * Qualquer tipo que for uma instância de `Eq` deve implementar `(==)` e `(/=)`.

         2. ##### Tornando um tipo uma instância de uma type-class:
            * Para tornar um tipo uma instância de uma type-class, você deve fornecer implementações para as funções requeridas.
            * Exemplo:
              ```haskell
              data Color = Red | Green | Blue

              instance Eq Color where
                  Red == Red = True
                  Green == Green = True
                  Blue == Blue = True
                  _ == _ = False
              ```
              * Aqui, o tipo `Color` é definido com três valores possíveis: `Red`, `Green` e `Blue`.
              * Ele é tornado uma instância de `Eq`, permitindo comparações de igualdade entre valores do tipo `Color`.

         3. ##### Usando type-classes:
            * Uma vez que um tipo é uma instância de uma type-class, você pode usar as funções definidas por essa type-class com aquele tipo.
            * Exemplo:
              ```haskell
              Red == Green  -- Resultado: False
              Blue /= Red   -- Resultado: True
              ```

         4. ##### Type-classes comuns em Haskell:
            * **`Eq`**: Para tipos que podem ser comparados por igualdade (`==`, `/=`).
            * **`Ord`**: Para tipos que possuem ordenação (`<`, `>`, `<=`, `>=`).
            * **`Show`**: Para tipos que podem ser convertidos para uma string (`show`).
            * **`Read`**: Para tipos que podem ser analisados a partir de uma string (`read`).
            * **`Num`**: Para tipos numéricos (`+`, `-`, `*`, etc.).
            * **`Functor`**: Para tipos que podem ser mapeados (por exemplo, listas).

         5. ##### Restrições de type-class:
            * Você pode restringir uma função para trabalhar apenas com tipos que são instâncias de uma type-class específica.
            * Exemplo:
              ```haskell
              add :: Num a => a -> a -> a
              add x y = x + y
              ```
              * A restrição `Num a =>` significa que `add` funciona apenas com tipos que são instâncias da type-class `Num`.

         6. ##### Exemplo: Criando uma type-class personalizada:
            * Você pode definir e usar uma type-class personalizada:
              ```haskell
              class Describable a where
                  describe :: a -> String

              data Animal = Dog | Cat

              instance Describable Animal where
                  describe Dog = "Este é um cachorro."
                  describe Cat = "Este é um gato."
              ```
              * Aqui, `Describable` é uma type-class que define uma função `describe`.
              * O tipo `Animal` é tornado uma instância de `Describable`, permitindo que você use a função `describe` com valores do tipo `Animal`.

        - #### Resumo:
         * Uma type-class define um conjunto de comportamentos (funções) que tipos podem implementar.
         * Tipos se tornam instâncias de uma type-class ao implementar suas funções.
         * Type-classes permitem polimorfismo, permitindo que funções trabalhem com qualquer tipo que satisfaça um comportamento específico.


<br>

- ## Programação Declarativa:
  - ### Idéia principal:
    Focar em "o quê" o programa deveria fazer, mais do que em "como" deveria fazer.
  - ### Recursos Chave / O Que É Programação Declarativa:
    - #### Sem fluxo de controle explícito:
      1. Programação declarativa evita o uso de instruções específicas de passo a passo ou de estruturas de controle como loops (<code>for</code>,<code>while</code>) ou condicionais (<code>if-else</code>).
      2. Ao invés disso, você descreve relacionamentos entre entradas (inputs) e saídas (outputs) ou as regras para suas computações. 
    - #### Expressões e declarações:
      1. Programas são escritos como uma série de expressões ou declarações que definem o que deve ser operado.
      2. Por exemplo, ao invés de se iterar através de uma lista para se operar uma soma, você pode simplesmente declarar a soma utilizando uma função como <code>sum</code> como argumento ao chamar uma função <code>map</code>.
    - #### Abstraindo detalhes de execução:
      1. O programador não precisa se preocupar com "como" as operações são performadas (ex. ao se preocupar em utilizar iteração, recursão ou sobre otimizações de performance). A própria linguagem ou mesmo a execução cuidam destes detalhes.
     
  - ### Programação Declarativa No Próprio Haskell:
    Haskell, é uma linguagem de programação declarativa, pois permite que você descreva a lógica das suas operações e programas computacionais sem especificar o fluxo de controle, Veja como isso funciona na prática:

     1. ### Como Haskell foca no 'O que deve ser feito' e não no 'Como':
       Em haskell, você define qual deve ser o resultado ao se utilizar de funções e expressões.
       - #### Exemplo:
         ```haskell
         let squares = map (^2) [1...5]
         ```
          - Isto declara que <code>squares</code> é o resultado do quadrado de cada número na lista <code>[1...5]</code>.
          - Você não especifica como iterar através desta lista, ou como aplicar a operação de expoente.

     2. ### Sem Loops:
       Haskell não tem as estruturas tradicionais de loops como <code>for</code> e <code>while</code>. Ao invés disso, utiliza-se recursão ou funções de ordem-maior como <code>map</code>, <code>filter</code>, e <code>fold</code> para expressar operções de forma declarativa.
       - #### Exemplo:
         ```haskell
         let evenNumbers = filter even [1..10]
         ```
    
     3. ### Funções puras:
       Como demonstrado antes, Haskell, assim como todas as linguagens de programação puramente funcionais, trata tudo como funções. Além disso, a programação declarativa na linguagem está diretamente ligada à sua natureza funcional. Como funções são puras, isso significa que não há efeitos colaterias, e que sempre produzem as mesmas saídas dadas as mesmas entradas. Isso tudo também torna mais fácil entender o que um programa faz.


     4. ### Capacidade de composição:
        A forma que Haskell é construída, encoraja a 'quebra' de problemas em funções menores e reutilizáveis que podem ser compostas juntas para resolver problemas maiores.
        - #### Exemplo:
          ```haskell
          let result = sum (filter even(map (^2) [1..10]))
          ```
          - Esta linha de código combina múltiplas funções de ordem-maior (<code>map</code>, <code>filter</code>, <code>sum</code>) para operar a soma dos quadrados de números pares de 1 a 10.
          - <code>map</code> - como já explicado anteriormente em: [map](https://github.com/marialmeida1/study-lp_seminariohaskell/edit/master/support_material/03-paradigms/FEATURES.md#exemplo-de-imutabilidade-em-a%C3%A7%C3%A3o), itera uma função (outra que não map´) passada como argumento, sobre cada elemento de uma lista.
          - <code>filter</code> - filtra de acordo com o argumento passado, no caso do código acima, os números pares (even).
          - <code>sum</code> - função de soma padrão de ordem-maior como qualquer outra linguagem.
          - <code>^x</code> - função de expoente padrão (onde x seria o número passado como argumento) de ordem-maior como qualquer outra linguagem.

- ## Computação Paralela:
  - ### Idéia principal:
    Haskell é uma linguagem funcional que facilita a computação paralela devido à sua imutabilidade, funções puras e suporte a bibliotecas robustas para paralelismo e concorrência.

  - ### Recursos chave:
    1. **Imutabilidade**:
       - Como os dados em Haskell são imutáveis, não há problemas de estado compartilhado ou condições de corrida, tornando a computação paralela mais segura.
    2. **Funções puras**:
       - Funções puras garantem que não há efeitos colaterais, permitindo que cálculos sejam executados em paralelo sem interferência.
    3. **Bibliotecas de paralelismo**:
       - Haskell oferece bibliotecas como `Control.Parallel`, `Control.Concurrent`, `async` e `STM` para facilitar a computação paralela e concorrente.

  - ### Exemplos:
    #### Usando `par` e `pseq` para paralelismo:
    ```haskell
    import Control.Parallel

    fib :: Int -> Int
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n - 1) + fib (n - 2)

    main :: IO ()
    main = do
        let x = fib 35 `par` fib 36 `pseq` (fib 35 + fib 36)
        print x
    ```
    - `par` inicia uma computação paralela para `fib 35`.
    - `pseq` garante que `fib 36` seja avaliado antes de combinar os resultados.

    #### Usando `Strategies` para paralelismo:
    ```haskell
    import Control.Parallel.Strategies

    main :: IO ()
    main = do
        let numbers = [1..1000000]
        let squares = map (^2) numbers `using` parList rseq
        print (sum squares)
    ```
    - `parList rseq` avalia os elementos da lista em paralelo.

    #### Concorrência com `forkIO`:
    ```haskell
    import Control.Concurrent

    main :: IO ()
    main = do
        forkIO $ putStrLn "Hello from thread 1"
        forkIO $ putStrLn "Hello from thread 2"
        threadDelay 1000000 -- Aguarda as threads terminarem
    ```

    #### Programação assíncrona com `async`:
    ```haskell
    import Control.Concurrent.Async

    main :: IO ()
    main = do
        result <- concurrently (return (fib 35)) (return (fib 36))
        print result
    ```

    #### Memória Transacional de Software (STM):
    ```haskell
    import Control.Concurrent.STM

    main :: IO ()
    main = do
        counter <- atomically $ newTVar 0
        atomically $ modifyTVar counter (+1)
        value <- atomically $ readTVar counter
        print value
    ```

  - ### Benefícios:
    - **Segurança**: A imutabilidade e as funções puras eliminam problemas de estado compartilhado.
    - **Escalabilidade**: Haskell aproveita múltiplos núcleos de CPU para melhorar o desempenho.
    - **Facilidade de uso**: As bibliotecas fornecem abstrações de alto nível para paralelismo e concorrência.

  - ### Aplicações:
    - Computação científica
    - Processamento de grandes volumes de dados
    - Sistemas distribuídos
    - Servidores web concorrentes

- ## Monads:
  - ### O que são Monads?
    Monads são uma abstração poderosa em Haskell que permitem lidar com efeitos colaterais, como entrada/saída, estado, ou manipulação de erros, de forma funcional e composicional. Elas encapsulam valores e computações, fornecendo uma maneira uniforme de encadear operações.

  - ### Estrutura de uma Monad:
    Uma Monad é definida por três componentes principais:
    1. **Função `return`**:
       - Envolve um valor em um contexto monádico.
       - Exemplo:
         ```haskell
         return 5 :: Maybe Int -- Resultado: Just 5
         ```
    2. **Operador `>>=` (bind)**:
       - Encadeia operações monádicas, passando o valor encapsulado para a próxima função.
       - Exemplo:
         ```haskell
         Just 5 >>= (\x -> Just (x + 1)) -- Resultado: Just 6
         ```
    3. **Leis das Monads**:
       - As Monads devem obedecer a três leis:
         1. **Lei da Identidade à Esquerda**: `return a >>= f` é equivalente a `f a`.
         2. **Lei da Identidade à Direita**: `m >>= return` é equivalente a `m`.
         3. **Lei da Associatividade**: `(m >>= f) >>= g` é equivalente a `m >>= (\x -> f x >>= g)`.

  - ### Exemplos de Monads Comuns:
    1. **Maybe**:
       - Lida com computações que podem falhar.
       - Exemplo:
         ```haskell
         safeDiv :: Int -> Int -> Maybe Int
         safeDiv _ 0 = Nothing
         safeDiv x y = Just (x `div` y)

         result = Just 10 >>= (\x -> safeDiv x 2) -- Resultado: Just 5
         ```
    2. **List**:
       - Representa computações não determinísticas.
       - Exemplo:
         ```haskell
         result = [1, 2] >>= (\x -> [x, x * 2]) -- Resultado: [1, 2, 2, 4]
         ```
    3. **IO**:
       - Lida com entrada/saída.
       - Exemplo:
         ```haskell
         main :: IO ()
         main = do
             putStrLn "Qual é o seu nome?"
             name <- getLine
             putStrLn ("Olá, " ++ name ++ "!")
         ```

  - ### Por que usar Monads?
    1. **Composição**:
       - Permitem encadear operações de forma limpa e legível.
    2. **Abstração**:
       - Encapsulam detalhes de manipulação de efeitos colaterais.
    3. **Reutilização**:
       - Funções monádicas podem ser reutilizadas em diferentes contextos.

  - ### Intuição:
    Uma maneira de entender Monads é pensar nelas como "caixas" que contêm valores e fornecem uma maneira de aplicar funções a esses valores sem "abrir" a caixa diretamente.

  - ### Conclusão:
    Monads são uma das características mais poderosas e desafiadoras de Haskell. Elas permitem que você escreva código funcional puro enquanto lida com efeitos colaterais de maneira controlada e composicional.














