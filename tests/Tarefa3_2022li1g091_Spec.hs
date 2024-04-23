module Tarefa3_2022li1g091_Spec where

import LI12223 
import Tarefa3_2022li1g091
import Test.HUnit

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test ["Teste 1" ~: Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])] ~=? obsmap 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])],
                                              "Teste 2" ~: [Tronco,Nenhum,Nenhum] ~=? mexelista (Rio 2) [Nenhum,Tronco,Nenhum],
                                              "Teste 3" ~: [Tronco,Nenhum,Nenhum] ~=? mexelista (Rio (-2)) [Nenhum,Nenhum,Tronco],
                                              "Teste 4" ~: 56 ~=? speed (Rio 56),
                                              "Teste 5" ~: 2 ~=? speedRio (Jogo (Jogador (2,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) 0,
                                              "Teste 6" ~: True  ~=? isRio (Rio 7),
                                              "Teste 7" ~: False ~=? isRio (Estrada 7),
                                              "Teste 8" ~: True  ~=? linhatronco (Jogo (Jogador (2,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) 0,
                                              "Teste 9" ~: False  ~=? linhatronco (Jogo (Jogador (1,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) 0,
                                              "Teste 10" ~: False ~=? linhatronco (Jogo (Jogador (0,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) 0,
                                              "Teste 11" ~: [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Carro, Carro, Nenhum]),(Rio 2, [Arvore,Arvore,Arvore,Nenhum,Nenhum])] ~=? mexecarrooutronco (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Rio 2, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]),
                                              "Teste 12" ~: Jogo (Jogador (0,0)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Nenhum,Nenhum,Tronco]),(Rio 1,[Nenhum,Nenhum,Nenhum,Tronco,Nenhum])])~=?  animaJogo (Jogo (Jogador (0,0)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) Parado,
                                              "Teste 13" ~: Jogo (Jogador (4,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Nenhum,Nenhum,Tronco]),(Rio 1,[Nenhum,Nenhum,Nenhum,Tronco,Nenhum])]) ~=?  animaJogo (Jogo (Jogador (2,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) Parado,
                                              "Teste 14" ~: Jogo (Jogador (2,0)) (Mapa 5 [(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Rio 2,[Nenhum,Nenhum,Nenhum,Nenhum,Tronco]),(Rio 1,[Nenhum,Nenhum,Nenhum,Tronco,Nenhum])]) ~=?  animaJogo (Jogo (Jogador (2,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Rio 2,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) (Move Cima),
                                              "Teste 15" ~: Jogo (Jogador (1,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Rio 2,[Nenhum,Nenhum,Nenhum,Nenhum,Tronco]),(Rio 1,[Nenhum,Nenhum,Nenhum,Tronco,Nenhum])]) ~=?  animaJogo (Jogo (Jogador (0,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Rio 2,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) (Move Direita),
                                              "Teste 16" ~: Jogo (Jogador (0,0)) (Mapa 5 [(Relva,[Nenhum,Nenhum,Arvore,Arvore,Arvore]),(Rio 2,[Nenhum,Nenhum,Nenhum,Nenhum,Tronco]),(Rio 1,[Nenhum,Nenhum,Nenhum,Tronco,Nenhum])]) ~=? animaJogo (Jogo (Jogador (0,0)) (Mapa 5 [(Relva,[Nenhum,Nenhum,Arvore,Arvore,Arvore]),(Rio 2,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) (Move Esquerda)]






 
 
 
 
