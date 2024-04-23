module Tarefa2_2022li1g091_Spec where

import LI12223
import Tarefa2_2022li1g091
import Test.HUnit

testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ test ["Teste 1" ~: [Estrada 0, Relva] ~=? proximosTerrenosValidos (Mapa 3 [(Rio 1 ,[Tronco,Tronco,Nenhum]),(Rio (-2) ,[Tronco,Nenhum,Nenhum]),(Rio 2 ,[Nenhum,Tronco,Nenhum]),(Rio (-1) ,[Tronco,Tronco,Nenhum])]),
                                              "Teste 2" ~: [Rio 0, Relva] ~=? proximosTerrenosValidos (Mapa 3 [(Estrada 1 ,[Carro,Carro,Nenhum]),(Estrada 2 ,[Carro,Carro,Nenhum]),(Estrada 3 ,[Carro,Carro,Nenhum]),(Estrada (-3) ,[Carro,Carro,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum])]),
                                              "Teste 3" ~:  [Estrada 0, Rio 0] ~=? proximosTerrenosValidos (Mapa 3 [(Relva ,[Arvore,Arvore,Nenhum]),(Relva ,[Arvore,Arvore,Nenhum]),(Relva ,[Arvore,Arvore,Nenhum]),(Relva ,[Arvore,Arvore,Nenhum]),(Relva,[Arvore,Arvore,Nenhum])]),
                                              "Teste 4" ~: [Rio 0, Estrada 0, Relva] ~=? proximosTerrenosValidos (Mapa 3 [(Rio 1 ,[Tronco,Tronco,Nenhum]),(Relva ,[Arvore,Nenhum,Nenhum]),(Rio 2 ,[Nenhum,Tronco,Nenhum]),(Rio (-3) ,[Tronco,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum])]),

                                              "Teste 5" ~: [Tronco,Nenhum] ~=? proximosObstaculosValidos 3 (Rio 0, [Nenhum,Tronco]),
                                              "Teste 6" ~:  [Nenhum] ~=? proximosObstaculosValidos 3 (Relva,[Arvore,Arvore]),
                                              "Teste 7" ~: [Nenhum] ~=? proximosObstaculosValidos 5 (Estrada 2, [Carro,Carro,Carro]),
                                              "Teste 8" ~: [] ~=? proximosObstaculosValidos 3 (Relva,[Arvore,Nenhum,Arvore]),

                                              "Teste 9" ~: [Tronco,Nenhum] ~=? checkRio1 3 [Nenhum,Tronco],
                                              "Teste 10" ~: [Nenhum] ~=? checkRio1 4 [Tronco,Tronco,Tronco],
                                              "Teste 11" ~: [Tronco] ~=? checkRio1 4 [Nenhum,Nenhum,Nenhum],

                                              "Teste 12" ~: [Carro,Nenhum] ~=? checkEstrada1 3 [Nenhum,Carro],
                                              "Teste 13" ~: [Nenhum] ~=? checkEstrada1 5 [Carro,Carro,Carro],

                                              "Teste 14" ~: [Arvore,Nenhum] ~=? checkRelva1 3 [Nenhum,Arvore],
                                              "Teste 15" ~: [Nenhum] ~=? checkRelva1 3 [Arvore,Arvore],

                                              "Teste 16" ~: False ~=? checkContiguo1 (Mapa 3 [(Rio 1 ,[Tronco,Tronco,Nenhum]),(Rio (-2) ,[Tronco,Nenhum,Nenhum]),(Rio 2 ,[Nenhum,Tronco,Nenhum]),(Rio (-3) ,[Tronco,Tronco,Nenhum])]),
                                              "Teste 17" ~: True ~=? checkContiguo1 (Mapa 3 [(Rio 1 ,[Tronco,Tronco,Nenhum]),(Relva ,[Arvore,Nenhum,Nenhum]),(Rio 2 ,[Nenhum,Tronco,Nenhum]),(Rio (-3) ,[Tronco,Tronco,Nenhum]),(Rio 1,[Tronco,Tronco,Nenhum])])]