# Principais características programáticas:

- ## Programação puramente funcional:
  * ### Idéia principal: 
    O principal ponto da programação puramente funcional, é que ele trata a computação como a avaliação de funções matemáticas. 
  * ### Recursos chave neste tipo de programação:
   1. #### Imutabilidade:
      Dados não podem ser alterados após serem criados.
      ##### Como a linguagem enforça a imutabilidade:
       - Variáveis:Em haskell, as variáveis são na verdade constantes. Quando se associa um valor à uma variável, ele pode ser mudado. 
       ```haskell
       x = 5
       x = 6 -- This would cause an error
       ```  
       - Estruturas de dados: Listas, tuplas e outras estruturas de dados em Haskell são imutáveis. Se você quiser "modificar" uma lista, você pode criar uma nova lista ao invés.
       ```haskell
       let list = [1, 2, 3]
       let newList = 0 : list
       -- Cria uma nova lista [0,1,2,3]
       ```

       
   2. #### Funções puras:
      Funções sempre produzem o mesmo "output" para o mesmo "input" sem produzir efeitos colaterais <code>Não modifica variáveis globais e nem printa para a tela</code>.
   

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

       
