-- Define uma lista infinita de números inteiros representando a sequência de Fibonacci.
-- A lista começa com os valores 0 e 1, e os próximos números são gerados somando os elementos da lista com sua versão deslocada.
{-A função zipWith aplica a operação de soma (+) elemento por elemento entre duas listas:
a lista fibonacci e sua versão deslocada (tail fibonacci), gerando os próximos números da sequência.-}
fibos :: [Integer]
fibos = 0 : 1 : zipWith (+) fibos (tail fibos)

main :: IO ()
-- Define a função principal do programa, que executa a lógica de exibição da sequência de Fibonacci.
main = print $ take 20 fibonacci
-- Imprime os primeiros 20 números da sequência de Fibonacci gerada.
