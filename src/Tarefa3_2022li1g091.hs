{- |
Module      : Tarefa3_2022li1g091
Description : Movimentação do personagem e obstáculos
Copyright   : Luís França <a104259@alunos.uminho.pt>
              João Brito <a104273@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}


module Tarefa3_2022li1g091 where

import LI12223

{- |A função 'animaJogo' está responsável por animar o mapa a cada momento que passa e o jogador dependendo da sua jogada.
As duas linhas que se encontram em forma de comentário é a parte pedida na segunda fase do projeto em relação aos troncos.
Estas estão desativadas no momento para melhorar a jogabilidade do jogo em si mas se for necessário podem ser ativadas outra vez.

== Exemplo de utilização
>>> animaJogo (Jogo (Jogador (2,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) (Move Cima)
 (Jogo (Jogador (2,0)) (Mapa 5 [(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Nenhum,Tronco,Nenhum])]))
>>> animaJogo (Jogo (Jogador (2,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) Parado 
 (Jogo (Jogador (3,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Nenhum,Tronco,Nenhum])]))
 -}
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x1,x2)) (Mapa y1 ((y2,y3):t))) z
    | linhaCarro j 0 = j
    | atropelado j z = Jogo (moveplayer j z) (Mapa y1 ((y2,y3):t))
    | (z == Parado || z == Move Esquerda && x1==0 || z == Move Direita && x1==(y1-1)|| z == Move Cima && x2==0 || z == Move Baixo && x2==length((y2,y3):t)-1) && linhatronco j 0 =
         Jogo (Jogador (x1+speedRio j 0,x2)) (Mapa y1 (mexecarrooutroncoupdt j z)) 
--    | z == Move Direita && linhatronco j 0 = Jogo (Jogador (x1+1+speedRio j 0,x2)) (Mapa y1 (mexecarrooutroncoupdt j z)) -> pedida na segunda fase 
--    | z == Move Esquerda && linhatronco j 0 = Jogo (Jogador (x1-1+speedRio j 0,x2)) (Mapa y1 (mexecarrooutroncoupdt j z))
    | z == Parado || z == Move Esquerda && x1==0 || z == Move Direita && x1==(y1-1)|| z == Move Cima && x2==0 || z == Move Baixo && x2==length((y2,y3):t)-1 = 
         Jogo (Jogador (x1,x2)) (Mapa y1 (mexecarrooutroncoupdt j z))

    | otherwise = Jogo (moveplayer j z) (Mapa y1 ((y2,y3):t)) -- Importante
    where j = Jogo (Jogador (x1,x2)) (Mapa y1 ((y2,y3):t))
          


{- |A função 'isRio' verifica se uma linha é um rio. -}
isRio :: Terreno -> Bool
isRio Relva = False
isRio (Estrada v) = False
isRio (Rio v) = True

{- |A função 'speed' devolve a velocidade de um terreno sem o tipo (Relva,Rio ou Estrada). -} 
speed :: Terreno -> Velocidade
speed Relva = 0
speed (Estrada v) = v
speed (Rio v) = v

{- |A função 'speedRio' devolve a velocidade do rio especifico em que o jogador se encontra.

== Exemplo de utilização
>>> speedRio (Jogo (Jogador (2,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) 0 
 1
-}
speedRio :: Jogo -> Int -> Velocidade 
speedRio (Jogo (Jogador(x1,x2)) (Mapa _ [])) i = 0
speedRio (Jogo (Jogador(x1,x2)) (Mapa y1 ((y2,y3):t))) i
    | isRio y2 && x2 == i = speed y2
    | otherwise = speedRio (Jogo (Jogador(x1,x2))(Mapa y1 t)) (i+1)

{- |A função 'linhatronco' verifica se o Jogador está em cima de um tronco.

== Exemplo de utilização
>>> linhatronco (Jogo (Jogador (2,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) 0 
 True
-}
linhatronco :: Jogo -> Int ->Bool 
linhatronco (Jogo  (Jogador(x1,x2)) (Mapa y1 [])) i = False
linhatronco (Jogo  (Jogador(x1,x2)) (Mapa y1 ((y2,y3):t))) i
    |  isRio y2 && x2 == i   && y3 !! x1 == Tronco = True
    |  otherwise = linhatronco (Jogo  (Jogador(x1,x2)) (Mapa y1 t)) (i+1)


-------Fase 2 
{- |A função 'moveplayer' move o jogador dependendo da jogada que faz.

== Exemplo de utilização
>>> moveplayer (Jogo (Jogador (2,0)) (Mapa 5 [(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) (Move Esquerda)
 Jogador (1,0)
-}
moveplayer :: Jogo -> Jogada -> Jogador
moveplayer (Jogo  (Jogador(x1,x2)) (Mapa y1 ((y2,y3):t))) z 
    | existeArvore (Jogo  (Jogador(x1,x2)) (Mapa y1 ((y2,y3):t))) z = Jogador(x1,x2)
    | z == Move Esquerda && x1 > 0 = Jogador(x1-1,x2)     
    | z == Move Cima && x2>0 = Jogador(x1,x2-1)
    | z == Move Baixo && x2 < (length ((y2,y3):t)-1) = Jogador(x1,x2+1)
    | z == Move Direita && x1 < y1-1 = Jogador(x1+1,x2)
     | (z == Parado || z == Move Esquerda && x1==0 || z == Move Direita && x1==(y1-1)|| z == Move Cima && x2==0 || z == Move Baixo && x2==length((y2,y3):t)-1) && linhatronco (Jogo  (Jogador(x1,x2)) (Mapa y1 ((y2,y3):t))) 0 =
         Jogador (x1+speedRio (Jogo  (Jogador(x1,x2)) (Mapa y1 ((y2,y3):t))) 0,x2)
    | otherwise = Jogador(x1,x2)


{- |A função 'linhaArvore' verifica se o Jogador está na mesma posição que uma árvore.-}
linhaArvore :: Jogo -> Int -> Bool
linhaArvore (Jogo  (Jogador(x1,x2)) (Mapa y1 [])) i = False
linhaArvore (Jogo  (Jogador(x1,x2)) (Mapa y1 ((y2,y3):t))) i
    | y2 == Relva && x2 == i   && y3 !! x1 == Arvore = True
    | otherwise = linhaArvore (Jogo  (Jogador(x1,x2)) (Mapa y1 t)) (i+1)  

{- |A função 'existeArvore' impede o Jogador de passar por árvores como se fosse um fantasma.-}
existeArvore :: Jogo -> Jogada -> Bool
existeArvore (Jogo (Jogador (x1,x2)) (Mapa y1 ((y2,y3):t))) z
    | z == Move Esquerda && linhaArvore (Jogo (Jogador (x1-1,x2)) (Mapa y1 ((y2,y3):t))) 0 = True         
    | z == Move Cima && linhaArvore (Jogo (Jogador (x1,x2-1)) (Mapa y1 ((y2,y3):t))) 0 = True
    | z == Move Baixo && linhaArvore (Jogo (Jogador (x1,x2+1)) (Mapa y1 ((y2,y3):t))) 0 = True
    | z == Move Direita && linhaArvore (Jogo (Jogador (x1+1,x2)) (Mapa y1 ((y2,y3):t))) 0 = True
    | otherwise = False 

{- |A função 'isEstrada' verifica se uma linha é uma estrada. -}
isEstrada :: Terreno -> Bool
isEstrada Relva = False
isEstrada (Estrada v) = True
isEstrada (Rio v) = False

{- |A função 'linhaCarro' verifica se o Jogador está na mesma posição que um carro.-}
linhaCarro :: Jogo -> Int -> Bool
linhaCarro (Jogo  (Jogador(x1,x2)) (Mapa y1 [])) i = False
linhaCarro (Jogo  (Jogador(x1,x2)) (Mapa y1 ((y2,y3):t))) i
    |  isEstrada y2 && x2 == i   && y3 !! x1 == Carro = True
    | otherwise = linhaCarro (Jogo  (Jogador(x1,x2)) (Mapa y1 t)) (i+1)

{- |A função 'atropelado' verifica se o jogador ao mover-se encontra-se na mesma posição que um carro. -}
atropelado :: Jogo -> Jogada -> Bool
atropelado (Jogo  (Jogador(x1,x2)) (Mapa y1 ((y2,y3):t))) z
    | linhaCarro (Jogo  (moveplayer (Jogo (Jogador (x1,x2)) (Mapa y1 ((y2,y3):t))) z) (Mapa y1 ((y2,y3):t))) 0 = True
    | otherwise = False
    where j = Jogo  (Jogador(x1,x2)) (Mapa y1 ((y2,y3):t))

{- |A função 'mexelistaupdt' move todos os elementos de uma lista um certo valor para a frente ou para trás e tambem essa lista de avançar
 se a qualquer ponto o jogador estiver na mesma posição que um carro.

== Exemplo de utilização 
>>> mexelistaupdt (Rio 1) [Nenhum,Nenhum,Tronco,Nenhum,Nenhum] (Jogo(Jogador (2,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) (Parado)
[Nenhum,Nenhum,Nenhum,Tronco,Nenhum]
-}


mexelistaupdt :: Terreno -> [Obstaculo] -> Jogo -> Jogada -> [Obstaculo] 
mexelistaupdt (Rio x) o (Jogo (Jogador (x1,x2)) (Mapa y1 ((y2,y3):t))) z
    | x==1 = last o : init o
    | x>1 = mexelistaupdt (Rio (x-1)) (last o : init o) j z
    | x==(-1) = tail o ++ [head o]
    | x<(-1) =  mexelistaupdt (Rio (x+1)) (tail o ++ [head o]) j z
    | otherwise = o
    where j = Jogo (Jogador (x1,x2)) (Mapa y1 ((y2,y3):t))    

mexelistaupdt (Estrada x) o (Jogo (Jogador (x1,x2)) (Mapa y1 ((y2,y3):t))) z
    | x>1 && not(linhaCarro j1 0)  = mexelistaupdt (Estrada (x-1)) (last o : init o) j1 Parado
    | x==1 || x>1 = last o : init o
    | x<(-1) && not(linhaCarro j2 0) =  mexelistaupdt (Estrada (x+1)) (tail o ++ [head o]) j2 Parado
    | x==(-1) || x<(-1) = tail o ++ [head o]
    | otherwise = o 
    where j1 = Jogo (moveplayer (Jogo (Jogador (x1,x2)) (Mapa y1 ((y2,y3):t))) z) (Mapa y1 ((y2,mexelistaupdt (Estrada 1) o j1 z):t))
          j2 = Jogo (moveplayer (Jogo (Jogador (x1,x2)) (Mapa y1 ((y2,y3):t))) z) (Mapa y1 ((y2,mexelistaupdt (Estrada (-1)) o j1 z):t))



{- |A função 'mexecarrooutroncoupdt' faz com que o mapa seja atualizado independentemente do Jogador, ou seja, mexe os carros e os troncos de acordo com a velocidade do respetivo terreno.

== Exemplo de utilização
>>> mexecarrooutroncoupdt (Jogo(Jogador (2,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) (Parado)
 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Nenhum,Tronco,Nenhum])]
-}

mexecarrooutroncoupdt :: Jogo -> Jogada -> [(Terreno,[Obstaculo])] 
mexecarrooutroncoupdt (Jogo (Jogador (x1,x2)) (Mapa _ [])) z = []
mexecarrooutroncoupdt (Jogo (Jogador (x1,x2)) (Mapa y1 ((y2,y3):t))) z
    | y2 == Relva = (y2,y3) : mexecarrooutroncoupdt (Jogo (Jogador (x1,x2)) (Mapa y1 t)) z
    | otherwise = (y2,mexelistaupdt y2 y3 (Jogo (Jogador (x1,x2)) (Mapa y1 ((y2,y3):t))) z) : mexecarrooutroncoupdt (Jogo (Jogador (x1,x2)) (Mapa y1 t)) z
