module Tarefa4_2022li1g091_Spec where

import LI12223
import Tarefa4_2022li1g091
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test ["Teste 1" ~: True  ~=? isRio (Rio 7),
                                              "Teste 2" ~: False ~=? isRio (Estrada 7),
                                              "Teste 3" ~: False  ~=? isEstrada (Rio 3),
                                              "Teste 4" ~: True ~=? isEstrada (Estrada 0),
                                              "Teste 5" ~: True  ~=? linhaCarro (Jogo (Jogador (2,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Estrada 2,[Nenhum,Nenhum,Carro,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) 0,
                                              "Teste 6" ~: False  ~=? linhaCarro (Jogo (Jogador (1,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Estrada 2,[Nenhum,Nenhum,Carro,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) 0,
                                              "Teste 7" ~: False ~=? linhaCarro (Jogo (Jogador (0,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Estrada 2,[Nenhum,Nenhum,Carro,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) 0,
                                              "Teste 8" ~: False  ~=? linhaagua (Jogo (Jogador (2,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) 0,
                                              "Teste 9" ~: True  ~=? linhaagua (Jogo (Jogador (1,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) 0,
                                              "Teste 10" ~: False ~=? linhaagua (Jogo (Jogador (0,0)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) 0,
                                              "Teste 12" ~: False ~=? jogoTerminou (Jogo (Jogador (1,0)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])),
                                              "Teste 13" ~: False ~=? jogoTerminou (Jogo (Jogador (2,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])),
                                              "Teste 14" ~: True ~=?  jogoTerminou (Jogo (Jogador (1,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Rio 2,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])), 
                                              "Teste 15" ~: True ~=?  jogoTerminou (Jogo (Jogador (2,1)) (Mapa 5 [(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Estrada 1,[Nenhum,Nenhum,Carro,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])),
                                              "Teste 16" ~: False ~=? jogoTerminou (Jogo (Jogador (0,0)) (Mapa 5 [(Relva,[Nenhum,Nenhum,Arvore,Arvore,Arvore]),(Estrada (-2),[Nenhum,Nenhum,Carro,Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])) ]
