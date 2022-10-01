{-
  Tarefa Lista de exercícios em Haskell
  Aluno:Matheus Monteiro
-}
--------------------------------------------------------------------------------------------
--exercício 1
--Escreva  uma  função  para  o  cálculo  dos  números  da  sequência  de  Fibonacci,  utilizando Haskell.  
fibonacci :: Int -> Int
fibonacci n
  | n == 0 = 0
  | n == 1 = 0
  | n == 2 = 1
  | otherwise = fibonacci (n-1) + fibonacci(n - 2)
--------------------------------------------------------------------------------------------
--exercício 2
{-
  Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor 
  Comum  (MDC)  de  Euclides  publicado  por  volta do  ano 300  AC.  Podemos simplificar  este 
  algoritmo  dizendo  que  dados  dois  inteiros  A  e  B,  o  MDC  entre  eles  será dado  pelo  valor 
  absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva 
  uma  função  para  o  cálculo  do  MDC  entre  dois  números  inteiros  positivos,  usando  o 
  algoritmo de Euclides conforme apresentado aqui, utilizando Haskell.  

-}

funçãoMDC :: Int -> Int -> Int
funçãoMDC a b
  | b == 0 = a
  | otherwise = funçãoMDC b (mod a b)


--------------------------------------------------------------------------------------------
--exercício 3
{-
Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos 
deste  número.  Exemplo:  dado  1234  a  função  deverá  devolver  10.  Utilizando  Haskell  e 
recursividade. 
-}

somaDigito :: Int -> Int
somaDigito a
  | a < 10 = a
  | otherwise = (a `mod`  10) + somaDigito(a `div`  10)

--------------------------------------------------------------------------------------------
--exercício 4
{-
Escreva  uma  função  que  devolva  a  soma  de  todos  os  números  menores  que  10000  que 
sejam múltiplos de 3 ou 5.
-}

somaDosDivisiveis ::  Int
somaDosDivisiveis = sum [x | x <- [1,2..1000],x `mod` 3 == 0, x`mod` 5 ==0]

-----------------------------------------------------------------------------------------------
--exercício 5
{-
Escreva  uma  função que,  recebendo  uma  lista  de  inteiros,  apresente  a  diferença  entre a 
soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade. 
-}

somaRec :: [Int] -> Int
somaRec [] = 0
somaRec a = head a + somaRec (tail a)

somaQuadrado :: [Int] -> Int
somaQuadrado a = (somaRec a)^2 - sum(map(^2) a)



-----------------------------------------------------------------------------------------------
--exercício 6
{-
O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma 
função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números 
primos menores que um determinado inteiro dado.  
-}

--funçãoEuler
------------------------------------------------------------------------------------------------
--exercício 7
{-
Nem  só  de  Fibonacci  vivem  os  exemplos  de  recursão.  Escreva  uma  função  que  devolva 
todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores 
que um inteiro dado. 
-}
funçãoLucas :: Int -> Int
funçãoLucas n 
  | n == 1 = 2
  | n == 2 = 1
  |otherwise = funçãoLucas(n-1) + funçãoLucas(n-2)
  

------------------------------------------------------------------------------------------------
--exercício 8
{-
Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3] 
devolva [3,2,1].  
-}

aoContrario :: [Int] ->[Int]
aoContrario a=  reverse (a)

------------------------------------------------------------------------------------------------
--exercício 9
{-
Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o 
produto destes valores sem usar o operador de multiplicação. 
-}

somaRecursiva :: Int -> Int -> Int
somaRecursiva a b
  |b == 1 = a
  |otherwise = a + somaRecursiva a (b-1)

------------------------------------------------------------------------------------------------
--exercício 10
{-
 Escreva uma função chamada comprimento que receba uma lista de  inteiros e devolva o 
comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule 
o comprimento de uma lista. 
-}





main = do
---------- Exercício 1 -------
  print(fibonacci 20)
---------- Exercício 2 -------
  print(funçãoMDC 2 10)
---------- Exercício 3 -------
  print(somaDigito 1234)
---------- Exercício 4 -------
  print(somaDosDivisiveis)
---------- Exercício 5 -------
  print(somaQuadrado [1,2,3])
---------- Exercício 6 -------

---------- Exercício 7 -------
  print(funçãoLucas 10)
--------- Exercício 8 --------
  print(aoContrario [1,2,3,4,5])
---------- Exercício 9 -------
  print(somaRecursiva 2 4)
---------- Exercício 10 -------
