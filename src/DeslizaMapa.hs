{- |
Module      : Tarefa2_2022li1g091
Description : Deslize do mapa
Copyright   : Luís França <a104259@alunos.uminho.pt>
              João Brito <a104273@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module DeslizaMapa where

import Tarefa1_2022li1g091
import Tarefa2_2022li1g091
import Tarefa3_2022li1g091
import Tarefa4_2022li1g091
import LI12223

{- | A função 'deslizaJogo' cria uma nova linha aleatoriamente gerada no topo do mapa e elimina a de baixo.-}
deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo r (Jogo (Jogador (x1,x2)) (Mapa n ((a,b):t))) = Jogo (Jogador (x1,(x2+1))) (estendeMapa (Mapa n (init ((a,b):t))) r)
