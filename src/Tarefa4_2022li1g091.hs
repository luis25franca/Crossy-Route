{- |
Module      : Tarefa4_2022li1g091
Description : Determinar se o jogo terminou
Copyright   : Luís França <a104259@alunos.uminho.pt>
              João Brito <a104273@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}

module Tarefa4_2022li1g091 where

import LI12223

{- |A função 'jogoTerminou' vai confirmar se o Jogador está morto ou se está vivo, ou seja, se o
jogador se encontrar fora do Mapa, se estiver na água ou se for atropelado então o
Jogador está morto e o jogo acaba. 

== Exemplo de utilização
>>> jogoTerminou (Jogo (Jogador (2,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])]))
 False
>>> jogoTerminou (Jogo (Jogador (1,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) 
 True
-}
jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (x1,x2)) (Mapa y1 ((y2,y3):t)))
    | x2 > (length ((y2,y3):t)-1) || x2 < 0 || x1 > (y1-1) || x1 < 0 = True
    | linhaCarro x 0 || linhaagua x 0 = True
    | otherwise = False
    where x = Jogo (Jogador (x1,x2)) (Mapa y1 ((y2,y3):t))    

{- |A função 'linhaCarro' verifica se o Jogador está na mesma posição que um carro, ou seja, se foi atropelado por um carro.

== Exemplo de utilização
>>> linhaCarro (Jogo (Jogador (2,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Estrada 1,[Nenhum,Nenhum,Carro,Nenhum,Nenhum])])) 0 
 True
-}
linhaCarro :: Jogo -> Int ->Bool
linhaCarro (Jogo  (Jogador(x1,x2)) (Mapa y1 [])) i = False
linhaCarro (Jogo  (Jogador(x1,x2)) (Mapa y1 ((y2,y3):t))) i
    |  isEstrada y2 && x2 == i   && y3 !! x1 == Carro = True
    | otherwise = linhaCarro (Jogo  (Jogador(x1,x2)) (Mapa y1 t)) (i+1)

{- |A função 'linhaagua' verifica se o Jogador não está em cima de um tronco quando está num rio.

== Exemplo de utilização
>>> linhaagua (Jogo (Jogador (1,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) 0 
 True
-}
linhaagua :: Jogo -> Int ->Bool 
linhaagua (Jogo  (Jogador(x1,x2)) (Mapa y1 [])) i = False
linhaagua (Jogo  (Jogador(x1,x2)) (Mapa y1 ((y2,y3):t))) i
    |  isRio y2 && x2 == i   && y3 !! x1 == Nenhum = True
    | otherwise = linhaagua (Jogo  (Jogador(x1,x2)) (Mapa y1 t)) (i+1)

{- |A função 'isRio' verifica se uma linha é um rio. -}
isRio :: Terreno -> Bool 
isRio Relva = False
isRio (Estrada v) = False
isRio (Rio v) = True

{- |A função 'isEstrada' verifica se uma linha é uma estrada. -}
isEstrada :: Terreno -> Bool 
isEstrada Relva = False
isEstrada (Estrada v) = True
isEstrada (Rio v) = False