module Tarefa1_2022li1g091_Spec where

import LI12223
import Tarefa1_2022li1g091
import Test.HUnit

testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ test ["Teste 1" ~: True ~=? mapaValido (Mapa 3 [(Rio 2, [Nenhum,Tronco,Tronco]),(Rio (-2) ,[Nenhum,Tronco,Tronco]),(Relva, [Nenhum,Arvore,Nenhum])]),
                                              "Teste 2" ~: False ~=? mapaValido (Mapa 3 [(Rio 2, [Nenhum,Tronco,Tronco]),(Rio 2 ,[Nenhum,Tronco,Tronco]),(Relva, [Nenhum,Nenhum,Nenhum])]),
                                              "Teste 3" ~: False ~=? mapaValido (Mapa 3 [(Estrada 2, [Carro,Carro,Carro]),(Rio (-2) ,[Nenhum,Tronco,Tronco]),(Relva, [Nenhum,Nenhum,Nenhum])]),
                                              "Teste 4" ~: True ~=? mapaValido (Mapa 4 [(Rio 2, [Nenhum,Tronco,Tronco,Nenhum]),(Rio (-2) ,[Nenhum,Tronco,Tronco,Tronco]),(Estrada 2, [Carro,Carro,Carro,Nenhum])]),
                                              "Teste 5" ~: False ~=? mapaValido (Mapa 7 [(Relva,[Arvore,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Arvore]),(Rio 1, [Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum])]),

                                              "Teste 6" ~: True ~=? checkTerreno (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),(Rio (-1), [Nenhum,Tronco,Tronco])]),
                                              "Teste 7" ~: False ~=? checkTerreno (Mapa 5 [(Estrada 2,[Carro,Carro,Carro,Carro,Nenhum]),(Estrada 1, [Carro,Nenhum,Carro,Nenhum,Carro])]),
                                              "Teste 8" ~: True ~=? checkTerreno (Mapa 3 [(Relva, [Arvore,Nenhum,Arvore]),(Relva, [Nenhum,Nenhum,Arvore]), (Relva, [Nenhum,Arvore,Arvore])]),
                                              
                                              "Teste 9" ~: False ~=? checkRio [Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum] 5,
                                              "Teste 10" ~: True ~=? checkRio [Tronco,Nenhum,Tronco,Tronco,Tronco] 5,

                                              "Teste 11" ~: False ~=? checkEstrada [Carro,Carro,Carro,Carro,Carro,Nenhum,Nenhum] 3,
                                              "Teste 12" ~: True ~=? checkEstrada [Carro,Nenhum,Carro,Carro,Nenhum] 3,

                                              "Teste 13" ~: True ~=? checkRelva [Arvore,Nenhum,Arvore,Nenhum],
                                              "Teste 14" ~: False ~=? checkRelva [Arvore,Nenhum,Carro,Nenhum],

                                              "Teste 15" ~: True ~=? checkNenhum (Mapa 3 [(Relva,[Nenhum,Arvore,Arvore]),(Relva,[Arvore,Nenhum,Arvore])]),
                                              "Teste 16" ~: False ~=? checkNenhum (Mapa 3 [(Relva,[Nenhum,Arvore,Arvore]),(Relva,[Arvore,Arvore,Arvore])]),
                                              
                                              "Teste 17" ~: True ~=? checkLargura  3 [Tronco,Tronco,Nenhum],
                                              "Teste 18" ~: False ~=? checkLargura 3 [Tronco,Tronco,Nenhum,Tronco],

                                              "Teste 19" ~: True ~=? checkDirecoes (Mapa 3 [(Rio (-1),[Tronco,Tronco,Nenhum]),(Rio 1,[Tronco,Tronco,Nenhum])]),
                                              "Teste 20" ~: False ~=? checkDirecoes (Mapa 3 [(Rio (-1),[Tronco,Tronco,Nenhum]),(Rio (-1),[Tronco,Tronco,Nenhum])]),

                                              "Teste 21" ~: False ~=? checkContiguo (Mapa 3 [(Rio 1 ,[Tronco,Tronco,Nenhum]),(Rio (-2) ,[Tronco,Nenhum,Nenhum]),(Rio 2 ,[Nenhum,Tronco,Nenhum]),(Rio (-3) ,[Tronco,Tronco,Nenhum]),(Rio 1,[Tronco,Tronco,Nenhum])]),
                                              "Teste 22" ~: False ~=? checkContiguo (Mapa 3 [(Estrada 1 ,[Carro,Carro,Nenhum]),(Estrada 2 ,[Carro,Carro,Nenhum]),(Estrada 3 ,[Carro,Carro,Nenhum]),(Estrada (-3) ,[Carro,Carro,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Estrada 1 ,[Carro,Carro,Nenhum]),(Estrada 2 ,[Carro,Carro,Nenhum])]),
                                              "Teste 23" ~: False ~=? checkContiguo (Mapa 3 [(Relva ,[Arvore,Arvore,Nenhum]),(Relva ,[Arvore,Arvore,Nenhum]),(Relva ,[Arvore,Arvore,Nenhum]),(Relva ,[Arvore,Arvore,Nenhum]),(Relva,[Arvore,Arvore,Nenhum]),(Relva ,[Arvore,Arvore,Nenhum]),(Relva ,[Arvore,Arvore,Nenhum])]),
                                              "Teste 24" ~: True ~=? checkContiguo (Mapa 3 [(Rio 1 ,[Tronco,Tronco,Nenhum]),(Relva ,[Arvore,Nenhum,Nenhum]),(Rio 2 ,[Nenhum,Tronco,Nenhum]),(Rio (-3) ,[Tronco,Tronco,Nenhum]),(Rio 1,[Tronco,Tronco,Nenhum])])]
