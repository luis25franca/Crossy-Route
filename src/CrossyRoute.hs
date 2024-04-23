{- |
Module      : Gloss
Description : Interface visual com Gloss
Copyright   : Luís França <a104259@alunos.uminho.pt>
              João Brito <a104273@alunos.uminho.pt>

Módulo para a realização da interface visual através do gloss do projeto de LI1 em 2022/23.
-}
module Gloss where 

import Tarefa1_2022li1g091
import Tarefa2_2022li1g091
import Tarefa3_2022li1g091
import Tarefa4_2022li1g091
import LI12223
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DeslizaMapa
import System.Random (Random(random))

{- | O novo tipo World define o estado Gloss -}
type World = (Menu, -- O tipo 'Menu' indica em que modo o jogo se encontra
             Jogo, -- O tipo 'Jogo' contem as informações referentes ao mapa e ao jogador.
             Int, -- Refere-se ao valor que será usado na geração aleatŕoia do mapa
             ShowTime, 
             [Picture], -- O tipo [Picture] refere-se às texturas que serão devolvidas no ecrã
             Jogada) -- O tipo 'Jogada' refere-se à última jogada efetuada através do teclado.


data Menu = Opcoes Opcao -- O estado 'Opcoes' entra em efeito no menu onde se pode escolher entre "Jogar" ou "Sair"
          | ModoJogo -- O estado 'ModoJogo' refere-se ao estado do jogo em si onde se toma controlo do jogador.
          | PerdeuJogo -- .. O estado 'PerdeuJogo' é referente ao ecrã que aparece quando se perde o Jogo, voltando depois para o menu.
          | PausaJogo -- .. O estado 'PausaJogo' é referente ao ecrã que aparece quando se pausa o Jogo.

data Opcao = Jogar -- Inicia o Jogo
            | Sair --Sai do Jogo

type ShowTime = Float


window :: Display
window = InWindow "Crossy Route" (1350, 1000) (500,500) 

background :: Color
background = greyN 0.8


fr :: Int
fr = 40


{- |O 'estadoGlossInicial' define o estado do Jogo ao ser inicializado a partir das texturas anteriomente dadas.-}
estadoGlossInicial :: [Picture] -> World
estadoGlossInicial i = (Opcoes Jogar, Jogo (Jogador (5,5)) (Mapa 9 [(Estrada 1 ,[Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva ,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore]),(Relva ,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore]),(Estrada 1 ,[Nenhum, Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Carro]),(Relva ,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore]),(Relva ,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore]),(Relva ,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore]),(Estrada 2 ,[Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Rio (-1) ,[Tronco,Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])]), 0, 0, i,Parado)

side = 100
{- | A função 'desenhaEstadoGloss' toma a informação do estado do Jogo e devolve uma imagem no ecrã.-}
desenhaEstadoGloss :: World -> Picture
desenhaEstadoGloss (PerdeuJogo, jogo, r, time,  [fundo,crossyRoute,play0,play1,exit0,exit1, pidgeyCima,pidgeyBaixo,pidgeyEsquerda,pidgeyDireita, carro, estrada, rio,relva,tabua,arvore,arbusto,taurosl,taurosr,gameover,pause,title,symbol],z) = Pictures [fundo,gameover]
desenhaEstadoGloss (Opcoes Jogar, jogo, r, time, [fundo,crossyRoute,play0,play1,exit0,exit1, pidgeyCima,pidgeyBaixo,pidgeyEsquerda,pidgeyDireita, carro, estrada, rio,relva,tabua,arvore,arbusto,taurosl,taurosr,gameover,pause,title,symbol],z) = Pictures [fundo,Translate (-50) 200 title,Translate 390 200 symbol, Translate 0 (-50) play1, Translate 0 (-200) exit0]--Translate 0 200 crossyRoute
desenhaEstadoGloss (Opcoes Sair, jogo, r, time,[fundo,crossyRoute,play0,play1,exit0,exit1, pidgeyCima,pidgeyBaixo,pidgeyEsquerda,pidgeyDireita, carro, estrada, rio,relva,tabua,arvore,arbusto,taurosl,taurosr,gameover,pause,title,symbol],z) = Pictures [fundo,Translate (-50) 200 title ,Translate 390 200 symbol, Translate (-2) (-49) play0,  Translate 38 (-201) exit1]
desenhaEstadoGloss (ModoJogo,Jogo (Jogador (x1,x2)) (Mapa l to), r, time, [fundo,crossyRoute,play0,play1,exit0,exit1, pidgeyCima,pidgeyBaixo,pidgeyEsquerda,pidgeyDireita, carro, estrada, rio,relva,tabua,arvore,arbusto,taurosl,taurosr,gameover,pause,title,symbol],z) = 
     Translate (-400) 400 (Pictures [ drawMapa side (Mapa l to) [ carro, estrada, rio,relva,tabua,arvore,arbusto,taurosl,taurosr], drawPlayer side (Jogador (x1,x2)) z  [pidgeyCima,pidgeyBaixo,pidgeyEsquerda,pidgeyDireita], Translate 925 (-75) (Scale 0.5 0.5 (text (mostraTempo time)))])
desenhaEstadoGloss (PausaJogo,Jogo (Jogador (x1,x2)) (Mapa l to), r, time, [fundo,crossyRoute,play0,play1,exit0,exit1, pidgeyCima,pidgeyBaixo,pidgeyEsquerda,pidgeyDireita, carro, estrada, rio,relva,tabua,arvore,arbusto,taurosl,taurosr,gameover,pause,title,symbol],z) = 
     Translate (-400) 400 (Pictures [ drawMapa side (Mapa l to) [ carro, estrada, rio,relva,tabua,arvore,arbusto,taurosl,taurosr], drawPlayer side (Jogador (x1,x2)) z  [pidgeyCima,pidgeyBaixo,pidgeyEsquerda,pidgeyDireita], Translate 925 (-75) (Scale 0.5 0.5 (text (mostraTempo time))),translate 400 (-350) pause])

{- |A funão 'drawPlayer' devolve a imagem do Jogador tendo em conta a direção em que está virado.-}
drawPlayer :: Float -> Jogador -> Jogada -> [Picture] -> Picture
drawPlayer side (Jogador (x,y)) z [pidgeyCima,pidgeyBaixo,pidgeyEsquerda,pidgeyDireita]
    | z == Move Cima = Translate x2 y2 $ Scale 3 3 pidgeyCima
    | z == Move Baixo = Translate x2 y2 $ Scale 3 3 pidgeyBaixo
    | z == Move Esquerda = Translate x2 y2 $ Scale 3 3 pidgeyEsquerda
    | z == Move Direita = Translate x2 y2 $ Scale 3 3 pidgeyDireita
    | otherwise = Translate x2 y2 $ Scale 3 3 pidgeyBaixo
  where x2 = side * fromIntegral x
        y2 = (-side) * fromIntegral y

{- |A função 'drawMapa' desenha o mapa do jogo.-}
drawMapa :: Float -> Mapa -> [Picture]-> Picture
drawMapa side (Mapa l []) i = Blank
drawMapa side (Mapa l ((a ,b):t)) i  = Pictures [drawLinha side a b i, Translate 0 (-side) ( drawMapa side (Mapa l t) i)]

{- |A função 'drawLinha' desenha individualmente cada linha do mapa tendo em conta o tipo de terreno.-}
drawLinha :: Float -> Terreno -> [Obstaculo] -> [Picture] -> Picture
drawLinha side _ [] [  carro, estrada, rio,relva,tabua,arvore,arbusto,taurosl,taurosr] = Blank
drawLinha side (Rio v) (t:ts) [ carro, estrada, rio,relva,tabua,arvore,arbusto,taurosl,taurosr] = Pictures [drawObstaculoRio side t [rio,tabua], Translate side 0 $ drawLinha side (Rio v) ts [carro, estrada, rio,relva,tabua,arvore,arbusto,taurosl,taurosr]]
drawLinha side Relva (t:ts) [carro, estrada, rio,relva,tabua,arvore,arbusto,taurosl,taurosr] = Pictures [drawObstaculoRelva side t [relva,arvore], Translate side 0 $ drawLinha side Relva ts [carro, estrada, rio,relva,tabua,arvore,arbusto,taurosl,taurosr]]
drawLinha side (Estrada v) (t:ts) [carro, estrada, rio,relva,tabua,arvore,arbusto,taurosl,taurosr] = Pictures [drawObstaculoEstrada side t (Estrada v) [estrada,taurosl,taurosr] , Translate side 0 $ drawLinha side (Estrada v) ts [carro, estrada, rio,relva,tabua,arvore,arbusto,taurosl,taurosr]]

{- |A função 'drawObstaculoRelva' desenha os obstáculos de uma linha do tipo relva-}
drawObstaculoRelva :: Float -> Obstaculo -> [Picture]-> Picture
drawObstaculoRelva side Nenhum [relva,arvore] = relva
drawObstaculoRelva side Arvore [relva,arvore] = Pictures [relva,arvore]

{- |A função 'drawObstaculoRio' desenha os obstáculos de uma linha do tipo rio-}
drawObstaculoRio :: Float -> Obstaculo -> [Picture]-> Picture
drawObstaculoRio side Nenhum [rio,tabua]= rio
drawObstaculoRio side Tronco [rio,tabua]= Pictures [rio, tabua]

{- |A função 'drawObstaculoEstrada' desenha os obstáculos de uma linha do tipo estrada-}
drawObstaculoEstrada :: Float -> Obstaculo -> Terreno -> [Picture]-> Picture
drawObstaculoEstrada side Nenhum (Estrada v) [estrada,taurosl,taurosr] = estrada
drawObstaculoEstrada side Carro (Estrada v) [estrada,taurosl,taurosr] 
    | v > 0 = Pictures [estrada,taurosr]
    | v < 0 = Pictures [estrada,taurosl]

mostraTempo :: ShowTime -> String
mostraTempo time = show (div sec 60) ++ ":" ++ show (mod sec 60)
            where sec = div (round time) 40

{- |A função 'controlos' altera o estado do gloss com o pressionar de certas teclas.-}
controlos :: Event -> World -> World
controlos (EventKey (SpecialKey k) Down _ _) (Opcoes Jogar,j, r, time,i,z)
    | k == KeyEnter = (ModoJogo, j, r, 0 , i,z)
    | k == KeyUp = (Opcoes Sair, j ,r+1, time , i,z)
    | k == KeyDown = (Opcoes Sair, j ,r+1,time , i,z)
controlos (EventKey (SpecialKey k) Down _ _) (Opcoes Sair, j , r, time , i,z)
    | k == KeyUp = (Opcoes Jogar,j, r+1, time,i,z)
    | k == KeyDown = (Opcoes Jogar,j, r+1, time,i,z)
    | k == KeyEnter = error "Exited Game"
controlos (EventKey (SpecialKey KeyEnter) Down _ _) (PausaJogo, Jogo(Jogador(x,y)) (Mapa l to), r, time,i,z) = (ModoJogo, Jogo(Jogador(x,y)) (Mapa l to),r, time, i,z)
controlos (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, j , r, time , i,z) = estadoGlossInicial i
controlos _ (ModoJogo, Jogo(Jogador(x,y)) (Mapa l to),r, time, textures,z) 
  | jogoTerminou (Jogo(Jogador(x,y)) (Mapa l to)) = (PerdeuJogo, Jogo (Jogador (5,5)) (Mapa 9 [(Estrada 1 ,[Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva ,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore]),(Relva ,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore]),(Estrada 1 ,[Nenhum, Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Carro]),(Relva ,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore]),(Relva ,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore]),(Relva ,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore]),(Estrada 2 ,[Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Rio (-1) ,[Tronco,Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])]),r,time,textures,Parado) 
controlos (EventKey k Down _ _) (ModoJogo, Jogo(Jogador(x,y)) (Mapa l to),r, time, i,z)
    | k == SpecialKey KeyUp || k == Char 'W' || k == Char 'w' = (ModoJogo,animaJogo  (Jogo(Jogador(x,y)) (Mapa l to)) (Move Cima), r+1, time,i,Move Cima)
    | k == SpecialKey KeyDown || k == Char 'S'|| k == Char 's' =  (ModoJogo,animaJogo  (Jogo(Jogador(x,y)) (Mapa l to)) (Move Baixo), r+2, time,i,Move Baixo)
    | k == SpecialKey KeyLeft || k == Char 'A' || k == Char 'a'=  (ModoJogo,animaJogo  (Jogo(Jogador(x,y)) (Mapa l to)) (Move Esquerda), r+3, time,i,Move Esquerda)
    | k == SpecialKey KeyRight || k == Char 'D'|| k == Char 'd' =  (ModoJogo,animaJogo  (Jogo(Jogador(x,y)) (Mapa l to)) (Move Direita), r+4, time,i,Move Direita) 
    | k == SpecialKey KeyEnter =   (PausaJogo, Jogo(Jogador(x,y)) (Mapa l to), r+4, time,i,z) 
controlos _ w = w

{- | A função 'reageTempoGloss' altera o estado do Gloss a cada frame que passa.-}
reageTempoGloss :: Float -> World -> World
reageTempoGloss b (ModoJogo, Jogo (Jogador (x1,x2)) (Mapa l to), r, time,  i,z) 
                                                                              | jogoTerminou (Jogo(Jogador(x1,x2)) (Mapa l to)) = (PerdeuJogo,Jogo(Jogador(1,1)) (Mapa l to),r+1, time+1,i,z) 
                                                                              | mod sec 40 == 0 = estendeMapaGloss (ModoJogo, animaJogo (Jogo (Jogador (x1,x2)) (Mapa l to)) Parado, r+1, time+1,  i,z)                                                                       
                                                                              | otherwise = estendeMapaGloss (ModoJogo, Jogo (Jogador (x1,x2)) (Mapa l to), r+1, time+1,  i,z)
                                                                                where sec = round time 
reageTempoGloss b (m, Jogo (Jogador (x1,x2)) (Mapa l to), r, time, i,z) = (m, Jogo (Jogador (x1,x2)) (Mapa l to), r+1, time, i,z)

{- | A função 'estendeMapa' verifica se as condições para que o mapa seja estendido estão cumpridas criando uma nova linha em cima do mapa e eliminando a de baixo-}
estendeMapaGloss :: World -> World
estendeMapaGloss (n, Jogo (Jogador (x1,x2)) (Mapa l ((a,b):t)), r, time,  i,z) | mod sec 120 == 0 || x2 <= 3 = (n, deslizaJogo r (Jogo (Jogador (x1,x2)) (Mapa l ((a,b):t))), r, time,  i,z)
                                                                               | otherwise = (n, Jogo (Jogador (x1,x2)) (Mapa l ((a,b):t)), rnd r, time,  i,z)
                                                                           where sec = round time
                                                                        

main :: IO ()
main = do
    fundo <- loadBMP "sprites/fundo.bmp"
    crossyRoute <- loadBMP "sprites/Crossy_Route.bmp"
    play0 <- loadBMP "sprites/Play0.bmp"
    play1 <- loadBMP "sprites/Play1.bmp"
    exit0 <- loadBMP "sprites/Exit0.bmp"
    exit1 <- loadBMP "sprites/Exit1.bmp"
    pidgeyCima <- loadBMP "sprites/pidgey_cima.bmp"
    pidgeyBaixo <-  loadBMP "sprites/pidgey_baixo.bmp"
    pidgeyEsquerda <- loadBMP "sprites/pidgey_esquerda.bmp"
    pidgeyDireita <- loadBMP "sprites/pidgey_direita.bmp"
    carro <- loadBMP "sprites/carro.bmp"
    estrada <- loadBMP "sprites/estrada.bmp"
    rio <- loadBMP "sprites/rio.bmp"
    relva <- loadBMP "sprites/grass.bmp"
    tabua <- loadBMP "sprites/wailmer.bmp"
    arvore <- loadBMP "sprites/tree.bmp"
    arbusto <- loadBMP "sprites/bush.bmp"
    taurosr <- loadBMP "sprites/TaurosR.bmp"
    taurosl <- loadBMP "sprites/TaurosL.bmp"
    gameover <- loadBMP "sprites/Game_Over.bmp"
    pause <- loadBMP "sprites/pause.bmp"
    title <- loadBMP "sprites/title.bmp"
    symbol <- loadBMP "sprites/symbol.bmp"
    play window --dimensões da janela do Jogo
         background --cor de fundo
         fr -- frame rate do jogo
         (estadoGlossInicial [Scale 1.1 1 fundo,crossyRoute,play0,play1,exit0,exit1,pidgeyCima, pidgeyBaixo, pidgeyEsquerda,pidgeyDireita,Scale 0.3 0.3 carro, Scale 0.21 0.21 estrada,Scale 0.37 0.37  rio,Scale 0.2 0.2 relva,Scale 4 4 tabua,Scale 0.4 0.4 arvore,Scale 2 2 arbusto,Scale 3.5 3.5 taurosl,Scale 3.5 3.5 taurosr ,Scale 1 1 gameover,pause, Scale 1.3 1.3 title,Scale 0.04 0.04 symbol]) -- estado inicial do gloss
         desenhaEstadoGloss -- função que devolve a Picture que será representada no ecrã
         controlos -- função que altera o estado do gloss tendo em conta as teclas pressionadas
         reageTempoGloss -- função que altera o estado do gloss tendo em conta a quantidade de frames que passaram

   