{- |
Module      : Tarefa2_2022li1g091
Description : Geração contínua de um mapa
Copyright   : Luís França <a104259@alunos.uminho.pt>
              João Brito <a104273@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g091 where
import Tarefa1_2022li1g091 

import LI12223

import System.Random

import Data.List ( isSuffixOf )

{- |A função 'randomIntsL' gera uma lista de números aleatórios a partir de uma seed e um valor de comprimento da lista.-}
randomIntsL :: Int -> Int -> [Int]
randomIntsL seed len = take len $ randoms (mkStdGen seed)

{- | A função 'rnd' devolve um número aleatŕoio que será utilisado pelas diversas funções.-}
rnd :: Int -> Int
rnd r = (head (randomIntsL r 1))                   

{- |A função 'estendeMapa' adiciona uma nova linha ao "topo" do mapa.-}
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa n ((a,b):t)) r | mapaValido (Mapa n (novaLinha (Mapa n ((a,b):t)) (rnd r) : (a,b) : t))  = (Mapa n (novaLinha (Mapa n ((a,b):t)) (rnd r) : (a,b) : t))
                                 | otherwise = (Mapa n ((novaLinha (Mapa n ((a,b):t)) (rnd (r+1)) : (a,b) : t)))



{- | A função 'novalinha' calcula um terreno aleatorio que será utilizado na nova linha.-}
novaLinha :: Mapa -> Int -> (Terreno, [Obstaculo])
novaLinha (Mapa n ((a,b):t)) r     | mod r 3 == 2 = addVelocidades (Mapa n ((head (proximosTerrenosValidos (Mapa n ((a,b):t))),[]):t)) (rnd r)
                                   | mod r 3 == 1 = addVelocidades (Mapa n ((last (take 2 (proximosTerrenosValidos (Mapa n ((a,b):t)))),[]):t)) (rnd r)
                                   | mod r 3 == 0 = addVelocidades (Mapa n ((last (proximosTerrenosValidos (Mapa n ((a,b):t))),[]):t)) (rnd r)

{- |A função 'addVelocidades' devolve um valor aleatório possível de uma velocidade dependenod do tipo do terreno.-}
addVelocidades :: Mapa -> Int -> (Terreno, [Obstaculo])
addVelocidades (Mapa n ((Rio 0,b):(Rio v1,_):t)) r | v1 < 0 = novoObstaculo (Mapa n ((Rio (1 + (mod (rnd r) (3))), b):t)) (rnd r)
                                                   | v1 > 0 = novoObstaculo (Mapa n ((Rio (-1 * (mod (rnd r) (3)) - 1), b):t)) (rnd r)
addVelocidades (Mapa n ((Rio 0,b):t)) r | mod (rnd r) 2 == 0 =  novoObstaculo (Mapa n ((Rio (1 + (mod (rnd r) (3))), b):t)) (rnd r)
                                        | mod (rnd r) 2 == 1 =  novoObstaculo (Mapa n ((Rio (-1 * (mod (rnd r) (3)) - 1), b):t)) (rnd r)
addVelocidades (Mapa n ((Estrada 0,b):t)) r | mod (rnd r) 2 == 0 =  novoObstaculo (Mapa n ((Estrada (1 + (mod (rnd r) (4))), b):t)) (rnd r)
                                            | mod (rnd r) 2 == 1 =  novoObstaculo (Mapa n ((Estrada (-1 * (mod (rnd r) (4)) - 1), b):t)) (rnd r)
addVelocidades (Mapa n ((a,b):t)) r = novoObstaculo (Mapa n ((a, b):t)) (rnd r)


{- | A função 'novoObstaculo' cria uma lista possível de obstaculos correspondete a um tipo de terreno de uma dada linha.-}
novoObstaculo :: Mapa -> Int -> (Terreno, [Obstaculo])
novoObstaculo (Mapa n ((a,b):t)) r     | proximosObstaculosValidos n (a,b) == [] = (a,b)
                                       | mod r 2 == 1 = novoObstaculo (Mapa n ((a,(head (proximosObstaculosValidos n (a, b)) : b)) : t)) (rnd r)
                                       | mod r 2 == 0 = novoObstaculo (Mapa n ((a,(last (proximosObstaculosValidos n (a, b)) : b)) : t)) (rnd r)

{- | A função 'proximosTerrenosValidos' devolve uma lista dos possíveis terrenos válidos tendo em conta os anteriores no mapa.

==Exemplo de utilização

>>>proximosTerrenosValidos (Mapa 3 [(Estrada 1 ,[Carro,Carro,Nenhum]),(Estrada 2 ,[Carro,Carro,Nenhum]),(Estrada 3 ,[Carro,Carro,Nenhum]),(Estrada (-3) ,[Carro,Carro,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum])])
[Rio 0, Relva]-}
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa n []) = [Rio 0, Estrada 0, Relva]
proximosTerrenosValidos (Mapa n ((Relva,b):t)) | checkContiguo1 (Mapa n ((Relva,b):t)) == False  = [Estrada 0, Rio 0]
                                               | otherwise = [Rio 0, Estrada 0, Relva]
proximosTerrenosValidos (Mapa n ((Rio v,b):t)) | checkContiguo1 (Mapa n ((Rio v,b):t)) == False  = [Estrada 0, Relva]
                                               | otherwise = [Rio 0, Estrada 0, Relva]
proximosTerrenosValidos (Mapa n ((Estrada v,b):t)) | checkContiguo1 (Mapa n ((Estrada v,b):t)) == False = [Rio 0, Relva]
                                                   | otherwise = [Rio 0, Estrada 0, Relva]


{- | A função 'proximosObstaculosValidos' devolve uma lista com os possíveis obstaculos validos tendo em conta o terreno e os obstaculos anteriores.-}
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos 1 _ = [Nenhum]
proximosObstaculosValidos n (Rio _, b) = checkRio1 n b
proximosObstaculosValidos n (Estrada _, b) = checkEstrada1 n b
proximosObstaculosValidos n (Relva , b) = checkRelva1 n b
                                        
{- | A função 'checkRio1' devolve uma lista com os possíveis obstaculos quando o terreno é do tipo rio.

==Exemplo de utilização

>>> checkRio1 4 [Tronco,Tronco,Tronco]
[Nenhum]
-}
checkRio1 :: Int -> [Obstaculo] -> [Obstaculo]
checkRio1 n [] = [Nenhum,Tronco]
checkRio1 n b | n == length b = []
              | isSuffixOf [Tronco,Tronco,Tronco,Tronco,Tronco] b = [Nenhum]
              | n == length b+1 && not(elem Nenhum b) = [Nenhum]
              | n == length b+1 && not(elem Tronco b) = [Tronco]
              | otherwise = [Tronco,Nenhum]

{- | A função 'checkEstrada1' devolve uma lista com os possíveis obstaculos quando o terreno é do tipo estrada.

==Exemplo de utilização

>>> checkEstrada1 5 [Carro,Carro,Carro]
[Nenhum]
-}
checkEstrada1 :: Int -> [Obstaculo] -> [Obstaculo]
checkEstrada1 n [] = [Nenhum,Carro]
checkEstrada1 n b | n == length b = []
                  | isSuffixOf [Carro,Carro,Carro] b = [Nenhum]
                  | n == length b+1 && not (elem Nenhum b) = [Nenhum]
                  | otherwise = [Carro,Nenhum]


{- | A função 'checkRelva1' devolve uma lista com os possíveis obstaculos quando o terreno é do tipo relva.

==Exemplo de utilização

>>> checkRelva1 3 [Nenhum,Arvore]
[Arvore,Nenhum]
-}
checkRelva1 :: Int -> [Obstaculo] -> [Obstaculo]
checkRelva1 n [] = [Nenhum,Arvore]
checkRelva1 n b | n == length b = []
                | n == ((length b)+1) && not(elem Nenhum b) = [Nenhum]
                | otherwise = [Arvore,Nenhum]

{- |A função 'checkContiguo' verifica se não existem mais de 3 rios contiguos, ou 4 relvas ou estradas contiguas

==Exemplo de utilização
>>> checkContiguo1 (Mapa 3 [(Rio 1 ,[Tronco,Tronco,Nenhum]),(Rio (-2) ,[Tronco,Nenhum,Nenhum]),(Rio 2 ,[Nenhum,Tronco,Nenhum]),(Rio (-3) ,[Tronco,Tronco,Nenhum])])
False
-}
checkContiguo1 :: Mapa -> Bool
checkContiguo1 (Mapa n []) = True
checkContiguo1 (Mapa n ((Rio _ ,_):(Rio _ ,_):(Rio _ ,_):(Rio _ ,_):t)) = False
checkContiguo1 (Mapa n ((Estrada _ ,_):(Estrada _ ,_):(Estrada _ ,_):(Estrada _ ,_):(Estrada _ ,_):t)) = False
checkContiguo1 (Mapa n ((Relva ,_):(Relva ,_):(Relva ,_):(Relva ,_):(Relva ,_):t)) = False
checkContiguo1 (Mapa n ((a,b):t)) = checkContiguo1 (Mapa n t)