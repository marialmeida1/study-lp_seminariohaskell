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

          -  ### O que é type-class em Haskell:
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











       
