-- Enzo Bottan Coutinho
module Main where

{-
1. Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um 
inteiro uma unidade maior que a entrada.
-}
soma1 :: Int -> Int
soma1 a = a+1
{-
2. Escreva  uma  função  chamada  sempre,  do  tipo  inteiro  que,  não  importando  o  valor  de 
entrada, devolva sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo. 
-}
sempre :: a -> Int
sempre a = 0
{-
3. Escreva  uma  função  chamada  treco  que  receba  três  valores  em  ponto  flutuantes  com 
precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro. 
-}
treco:: Double -> Double -> Double -> Double
treco a b c = (a + b) * c

{-
4. Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números 
inteiros. 
-}
resto :: Int -> Int -> Int
resto a b = mod a b

{-
5. Escreva uma função chamada precoMaior que devolva o maior valor entre quatro valores 
monetários. 
-}
precoMaior :: Double -> Double -> Double -> Double -> Double
precoMaior a b c d = if a >= b && a >= c && a >= d then a
else if b >= a && b >= c && b >= d then b
else if c >= a && c >= b && c >= d then c
else d

{-
6. Escreva uma função chamada impar que devolva True, sempre que o resultado do produto 
de dois números inteiros for ímpar.  
-}
impar :: Int -> Int -> Bool
impar a b = if (mod (a*b) 2 /= 0) then True else False

{-
7. Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟∷(𝐼𝑛𝑡,𝐼𝑛𝑡). Escreva 
uma função em Haskell que devolva a soma dos componentes de um par de inteiros.  
-}
par :: (Int, Int) -> Int
par (a,b) = a + b

{-
8. Escreva uma função em Haskell que receba números reais (double) e devolva o resultado 
da equação 𝑥2 +𝑦2 +𝑧.  
-} 
equacao :: Double -> Double -> Double -> Double
equacao x y z = x^2 + (y/2) + z

{- 
9. Escreva uma função em Haskell chamada diagnostico que receba o peso e a altura do aluno 
e imprima um diagnóstico de obesidade, segundo a tabela que pode ser encontrada no link: 
Sobrepeso,  obesidade  e  obesidade  mórbida:  entenda  a  diferença  entre  os  três  termos 
(cuidadospelavida.com.br).  Observe  que  este  diagnóstico  é  meramente  estatístico  e  não 
tem nenhum valor real, está sendo usado nesta questão apenas para a definição das faixas. 
Todo e qualquer diagnóstico deve ser feito por um profissional médico.

Abaixo de 17  –  Muito abaixo do peso
Entre 17 e 18,49   –  Abaixo do peso
Entre 18,5 e 24,99  –    Peso normal
Entre 25 e 29,99   –  Sobrepeso
Entre 30 e 34,99   –  Obesidade Ieve
Entre 35 e 39,99   –  Obesidade severa
Acima de 40 – Obesidade mórbida
-}
diagnostico :: Double -> Double -> String
diagnostico a b = if ((a/(b^2)) <= 17) then "Muito abaixo do peso" 
else if  ((a/(b^2)) > 17 && ((a/(b^2))) <= 18.49) then "Abaixo do peso"
else if  ((a/(b^2)) > 18.5 && ((a/(b^2))) <= 24.99) then "Peso normal"
else if  ((a/(b^2)) > 25 && ((a/(b^2))) <= 29.99) then "Sobrepeso"
else if  ((a/(b^2)) > 30 && ((a/(b^2))) <= 34.99) then "Obesidade Ieve"
else if  ((a/(b^2)) > 35 && ((a/(b^2))) <= 39.99) then "Obesidade severa"
else "Obesidade mórbida"

{- 
10. Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o 
ano for bisexto sabendo que anos bissextos obedecem a seguinte regra:  
𝑇𝑜𝑑𝑜𝑠 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠𝑒𝑗𝑎𝑚 𝑑𝑖𝑣𝑖𝑠í𝑣𝑒𝑖𝑠 𝑝𝑜𝑟 4 
      𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 100 
            𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 400 
1997 não é bissexto, 1900 não é bissexto e 2000 é bissexto.
-}
bissexto :: Int -> Bool
bissexto a = if (mod a 4) == 0 && (mod a 100) /= 0 || (mod a 400) == 0 then True
else False

main :: IO()
main = do

  putStrLn ("Func. 1: entrada:1; resultado:" ++ show (soma1 1))
  putStrLn ("Func. 2: entrada:5; resultado:" ++ show (sempre 5))
  putStrLn ("Func. 3: entrada:2.1 2.2 2.3; resultado:" ++ show (treco 2.1 2.2 2.3))
  putStrLn ("Func. 4: entrada:6 2; resultado:" ++  show (resto 6 2))
  putStrLn ("Func. 5: entrada:1.5 2.3 3.4 4.7; resultado:" ++  show (precoMaior 1.5 2.3 3.4 4.7))
  putStrLn ("Func. 6: entrada:1 2 3 4; resultado:" ++  show (impar 1 2))
  putStrLn ("Func. 7: entrada:(1,2); resultado:" ++ show (par(1,2)))
  putStrLn ("Func. 8: entrada:2 3 4; resultado:" ++ show (equacao 2 3 4))
  putStrLn ("Func. 9: entrada:73.54 1.80; resultado:" ++ show (diagnostico 73.54 1.80))
  putStrLn ("Func. 10: entrada:2000; resultado:" ++ show (bissexto 2000))
  

