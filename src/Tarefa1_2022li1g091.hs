{- |
Module      : Tarefa1_2022li1g091
Description : Validação de um mapa
Copyright   : Luís França <a104259@alunos.uminho.pt>
              João Brito <a104273@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g091 where

import LI12223
import Data.List

{- |A função 'mapaValido' é a função principal deste documento que verifica se um dado mapa é valido através de diversas funções auxiliares

== Exemplo de utilização

>>> mapaValido (Mapa 3 [(Rio 2, [Nenhum,Tronco,Tronco]),(Rio -2 ,[Nenhum,Tronco,Tronco]),(Relva, [Nenhum,Nenhum,Nenhum])])
True
>>> mapaValido (Mapa 3 [(Rio 2, [Nenhum,Tronco,Tronco]),(Rio 2 ,[Nenhum,Tronco,Tronco]),(Relva, [Nenhum,Nenhum,Nenhum])])
False
>>> mapaValido (Mapa 3 [(Estrada 2, [Carro,Carro,Carro]),(Rio -2 ,[Nenhum,Tronco,Tronco]),(Relva, [Nenhum,Nenhum,Nenhum])])
False  -}
mapaValido :: Mapa -> Bool
mapaValido (Mapa n ((a,b):t)) = checkTerreno (Mapa n ((a,b):t)) && checkNenhum (Mapa n ((a,b):t)) && checkContiguo (Mapa n ((a,b):t)) && checkPassavel (Mapa n ((a,b):t))

{- |A função 'checkTerreno' verifica se um dado tereno é valido tendo em conta os que o precedem e os obstáculos contidos nea mesma linha -}
checkTerreno :: Mapa -> Bool
checkTerreno (Mapa n []) = True
checkTerreno (Mapa n ((Rio v ,b):t)) | checkRio b 6 && checkLargura n b && checkDirecoes (Mapa n ((Rio v,b):t)) && elem Tronco b = checkTerreno (Mapa n t)
                                     | otherwise = False
checkTerreno (Mapa n ((Estrada _ ,b):t)) | checkEstrada b 4 && checkLargura n b = checkTerreno (Mapa n t)
                                         | otherwise = False
checkTerreno (Mapa n ((Relva ,b):t)) | checkRelva b && checkLargura n b = checkTerreno (Mapa n t)
                                     | otherwise = False

{- |A função 'checkRio' verifica se os obstáculos numa linha com o terreno "Rio" são válidos

== Exemplo de utilização
>>> checkRio [Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum] 5
False
>>> checkRio [Tronco,Nenhum,Tronco,Tronco,Tronco] 5
True
-}
checkRio :: [Obstaculo] -> Int -> Bool
checkRio [] u = True
checkRio _ 0 = False
checkRio (b:bs) u
                | b == Nenhum = checkRio bs 6
                | b == Tronco = checkRio bs (u-1)
                | otherwise = False
 
{- |A função 'checkEstrada' verifica se os obstáculos numa linha com o terreno "Estrada" são válidos

==Exemplo de utilização

>>> checkEstrada [Carro,Nenhum,Carro,Carro,Nenhum] 3
True
-}
checkEstrada :: [Obstaculo] -> Int -> Bool
checkEstrada [] u = True
checkEstrada _ 0 = False
checkEstrada (b:bs) u
                | b == Nenhum = checkEstrada bs 4
                | b == Carro = checkEstrada bs (u-1)
                | otherwise = False

{- |A função 'checkRelva' verifica se os obstáculos numa linha com o terreno "Relva" são válidos

== Exemplo de utilização

>>> checkRelva [Arvore,Nenhum,Arvore,Nenhum]
True
-}
checkRelva :: [Obstaculo] -> Bool
checkRelva [] = True
checkRelva (b:bs)
                | b == Nenhum  = checkRelva bs
                | b == Arvore  = checkRelva bs
                | otherwise = False

{- |A função 'checkNenhum' verifica,linha a linha, se existe pelo menos um obstaculo "Nenhum" em todas as linhas

==Exemplo de utilização

>>> checkNenhum (Mapa 3 [(Relva,[Nenhum,Arvore,Arvore]),(Relva,[Arvore,Arvore,Arvore])])
False
-}
checkNenhum :: Mapa -> Bool
checkNenhum (Mapa n []) = True
checkNenhum (Mapa n ((a,b):t)) | elem Nenhum b = checkNenhum (Mapa n t)
                               | otherwise = False


{- | A função 'checkLargura' verifica se o comprimento da lista de obstaculos é igual ao valor da largura
== Exemplo de utilização

>>> checkLargura 3 [Tronco,Tronco,Nenhum,Tronco]
False
-}
checkLargura :: Largura -> [Obstaculo] -> Bool
checkLargura n b | n == length b = True
                 | otherwise = False

{- | A função 'checkDirecoes' verifica se linhas contiguas do terreno "Rio" possuem direções diferentes

==Exemplo de utilização

>>> checkDirecoes (Mapa 3 [(Rio (-1),[Tronco,Tronco,Nenhum]),(Rio 1,[Tronco,Tronco,Nenhum])])
True
-}
checkDirecoes :: Mapa -> Bool
checkDirecoes (Mapa n ((Rio v1,_):(Rio v2,_):t)) 
                                                | v1 < 0 && v2 > 0 = True
                                                | v1 > 0 && v2 < 0 = True
                                                | otherwise = False
checkDirecoes (Mapa n ((Rio v1,_):t)) = True

{- |A função 'checkContiguo' verifica se não existem mais de 4 rios contiguos, ou 5 relvas ou estradas contiguas 

== Exemplo de utilização

>>> checkContiguo (Mapa 3 [(Rio 1 ,[Tronco,Tronco,Nenhum]),(Relva ,[Arvore,Nenhum,Nenhum]),(Rio 2 ,[Nenhum,Tronco,Nenhum]),(Rio (-3) ,[Tronco,Tronco,Nenhum]),(Rio 1,[Tronco,Tronco,Nenhum])])
True
-}          
checkContiguo :: Mapa -> Bool
checkContiguo (Mapa n []) = True
checkContiguo (Mapa n ((Rio _ ,_):(Rio _ ,_):(Rio _ ,_):(Rio _ ,_):(Rio _,_):t)) = False
checkContiguo (Mapa n ((Estrada _ ,_):(Estrada _ ,_):(Estrada _ ,_):(Estrada _ ,_):(Estrada _ ,_):(Estrada _ ,_):t)) = False
checkContiguo (Mapa n ((Relva ,_):(Relva ,_):(Relva ,_):(Relva ,_):(Relva ,_):(Relva ,_):t)) = False
checkContiguo (Mapa n ((a,b):t)) = checkContiguo (Mapa n t)

{- |A função 'checkPassavel' verifica se um dado mapa é passável ou seja pode ser completado sem becos sem fim.-}
checkPassavel :: Mapa -> Bool
checkPassavel (Mapa n ((Relva, x): (Relva,y):t)) = verificaSeIgual (elemIndices Nenhum x) (elemIndices Nenhum y) && checkPassavel (Mapa n ((Relva,y):t))
checkPassavel (Mapa n ((Estrada v1, x): (Estrada v2,y):t)) | v1 == v2 = verificaSeIgual (elemIndices Nenhum x) (elemIndices Nenhum y) && checkPassavel (Mapa n ((Estrada v2,y):t))
                                                           | otherwise = checkPassavel (Mapa n ((Estrada v2,y):t))
checkPassavel (Mapa n ((Rio v1, x): (Estrada v2,y):t)) | v1 == v2 = (elemIndices Tronco x) /= (elemIndices Carro y) && checkPassavel (Mapa n ((Estrada v2,y):t))
                                                                 | otherwise = checkPassavel (Mapa n ((Estrada v2,y):t))
checkPassavel (Mapa n ((Estrada v1, x): (Rio v2,y):t)) | v1 == v2 = (elemIndices Carro x) /= (elemIndices Tronco y) && checkPassavel (Mapa n ((Rio v2,y):t))
                                                                 | otherwise = checkPassavel (Mapa n ((Rio v2,y):t))
checkPassavel (Mapa n (x:xs)) = checkPassavel (Mapa n (xs))
checkPassavel (Mapa n []) = True

{- | A função 'verificaSeIgual' verifica se existes uma coluna livre de obstáculos no mapa para que este seja passável.-}
verificaSeIgual :: [Int] -> [Int] -> Bool
verificaSeIgual [] _ = False
verificaSeIgual (x:xs) y | elem x y = True
                         | elem x y == False = verificaSeIgual xs y
