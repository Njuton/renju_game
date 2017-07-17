module Graphics where

import Types
import Files
import Logic
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Matrix

             
--запуск игры
go :: World -> IO ()
go world = play (InWindow "Game Rejnzu" (500,500) (0,0)) 
               white 
               1
               world 
               convert 
               Graphics.handle
               update

--изменяет таймер
update ::Float -> World -> World
update _ (World a b (W x) d e f g h) = (World a b (W x) d e f g h)
update _ (World a state c d e (x,y) m (Limit,y1,z1,p)) | (p == True) = (World a state c d e (x,y) m (Limit,y1,z1,p))
                                                       | (x == 0) || (y == 0) = (World a state c d e (x,y) m (Limit,y1,z1,p))
                                                       | (x == 1) = (World a state (W Red) d e (0,y) m (Limit,y1,z1,p))
                                                       | (y == 1) = (World a state (W Black) d e (x,0) m (Limit,y1,z1,p))
                                                       | otherwise            = (World a state c d e (f state) m (Limit,y1,z1,p))
                                                         where
                                                          f Red     = (x, y-1)
                                                          f Black   = (x-1, y)
update _ (World a b c d e f g h) = (World a b c d e f g h)  --если режим неограниченный по времени, то ничего не делаем

--переводит внутреннее представление мира в картинку
convert :: World -> Picture
convert (World m _ w p _ t menu (ti,ha,mo,pa)) =
                           Pictures $
                           drawPic w p ++ 
                           time ti t p    ++ 
                           mainDrawField m ++
                           drawMenu p menu (ti,ha,mo,pa)
                           
--отрисовка главного меню и меню опций (главная функция)
drawMenu :: [Picture] -> Menu ->(Time, Hard, Mode, Pause)->[Picture]
drawMenu p (Main 0) _ = drawMain p                                                       --главное меню
drawMenu p (Main n) _ = (zipWith (\dy i -> translate offsetX (offsetY + dy) $ p !! i)    --главное меню с выбором категории
                              [0,c]
                              [10,9]) ++ (tail $ drawMain p)
                      where
                      c = (fromIntegral sizeField) / 2 * sizeCell - 140 - 60 * fromIntegral(n - 1)
drawMenu pic Opt (t,h,m,_) = let                                                        --меню опций, отрисовка галочки
                                one = case t of
                                            Limit     -> c 35 (-120)
                                            No_limit  -> c 130 (-120)
                                two = case h of
                                             Hard     -> c 35 (-200)
                                             Easy     -> c 130 (-200)
                                three = case m of
                                             Hum_Hum  -> c 35 (-280)
                                             Hum_Comp -> c 130 (-280)      
                              in drawOpt pic ++ [one,two,three]    
                                where
                                c x y = translate (offsetX + x) (offsetY + (fromIntegral sizeField) / 2 * sizeCell + y ) $ pic !! 16                           
drawMenu _ _ _ = [Blank]

--отрисовка главного меню
drawMain :: [Picture] -> [Picture]
drawMain p =  (zipWith (\dy i -> pic 0 dy i) 
                      [-c,-60,-140,-200,-260] 
                      [10,14,13,19,17]) ++
              [pic 170 (-50) 23]
              where 
              pic x y i = translate (offsetX + x) (offsetY + (fromIntegral sizeField) / 2 * sizeCell + y ) $ p !! i
              c         = (fromIntegral sizeField) / 2 * sizeCell
--отрисовка меню опций
drawOpt :: [Picture] -> [Picture]
drawOpt   p =  
               zipWith (\dx pic -> translate dx 0 pic) 
                                [0, 170,  0,-120,  35,130, -100 ,35, 130, -110,  35, 130,-150]
                       (zipWith (\dy i -> translate offsetX (offsetY + (fromIntegral sizeField) / 2 * sizeCell + dy) $ p !! i)
                                [-c,-50,-60,-120,-120,-120,-200,-200,-200,-280,-280,-280,-330]
                                [10, 23, 18,  22,  20,  21,  12,  11,  24,  15,  25,  26,   8])
               where
               c = (fromIntegral sizeField) / 2 * sizeCell

--отрисовка "часов"
time :: Time -> PointI -> [Picture] -> [Picture]
time Limit (x,y) p = zipWith (\ z dx -> translate (offsetX + dx) (offsetY + (fromIntegral sizeField) / 2 * sizeCell) $ Scale 0.3 0.3 $ Text $ show z)
             [x,y] [c2,c1]
             ++
             zipWith (\ dx i -> translate (offsetX + dx) (offsetY + (fromIntegral sizeField) / 2 * sizeCell + 50) $ p !! i)
             [c4,c3] [6,7]
             where 
             c1         = (fromIntegral sizeField) / 2 * sizeCell - 35
             c2         = - c1 - 50
             c3         = c1 + 20
             c4         = c2 + 30
time _ _ _ = [Blank]
			 
--отрисовка того, кто выиграл
drawPic :: Win -> [Picture] -> [Picture]
drawPic x p = case x of
              None ->         zipWith(\dy i -> translate offsetX (offsetY + dy) $ p !! i)
                                      [1,220,270] 
                                      [5,4,0]
                                
              _     ->        zipWith(\dy i -> translate offsetX (offsetY + dy) $ p !! i)
                                     [1,270,220]
                                     [5,0,msg x]
              where
                  msg (W Black) = 2
                  msg (W Red)   = 1
                  msg (Tie)     = 3 

--возвращает список - строку матрицы 
getRow :: Int -> Matrix Cell -> [Cell] 
getRow n m = [m ! (i,j) | i <- [n]
                          , j <- [1 .. (ncols m)]]
                          

--отрисовка одной ячейки (позиции - положение квадратика относительно центра) и левой верхней фишечки, если она есть
drawCell :: Float -> Float -> Cell -> [Picture]
drawCell pos_x pos_y s = case s of
                             Nothing -> [reckWire] 
                             (Just x) -> [reckWire
                                         ,Translate 
                                         (pos_x - (sizeCell / 2))
                                         (pos_y + (sizeCell / 2)) $
                                         Color (col x) $ circleSolid (sizeCell / 2)]
                         where
                         reckWire =  translate 
                                     pos_x 
                                     pos_y $
                                     rectangleWire sizeCell sizeCell
                         col Black = black
                         col Red   = red

--отрисовка строки
--pos_x pos_y положение самого первого квадратика строки относительно центра

drawRow :: Float -> Float -> [Cell] -> [Picture]
drawRow pos_x pos_y [(Just x)] = [Translate 
                                                   (pos_x - (sizeCell / 2)) 
                                                   (pos_y + (sizeCell / 2)) $
                                                   Color (col x) $ circleSolid (sizeCell / 2)]
                                 where 
                                 col Black = black
                                 col Red   = red
drawRow _     _     [Nothing]         =     [Blank]

drawRow pos_x pos_y l                 =     (drawCell pos_x pos_y $ head l) 
                                                ++ (drawRow (pos_x + sizeCell) pos_y $ tail l)

--отрисовка всей сетки
mainDrawField :: Field -> [Picture]
mainDrawField m  =                            drawField 
                                                        (offsetX - sizeCell * fromIntegral (((nrows m) - 1) `div` 2)) 
                                                        (offsetY + sizeCell * fromIntegral (((nrows m) - 1) `div` 2)) 
                                                        ((nrows m) - 1) 
                                                        m

--n количество оставшихся на отрисовку строк 

drawField :: Float -> Float -> Int -> Matrix Cell -> [Picture]
drawField pos_x pos_y 0 m          =     drawLastRow 
                                                        (pos_x - (sizeCell / 2)) 
                                                        (pos_y + (sizeCell / 2)) 
                                                        $ Graphics.getRow 1 m
drawField pos_x pos_y n m          =     (drawRow pos_x pos_y $ Graphics.getRow 1 m) 
                                                ++ (drawField 
                                                        pos_x (pos_y - sizeCell) 
                                                        (n - 1) 
                                                        (submatrix 2 (nrows m) 1 (ncols m) m)) 

--отрисовка последних фишичек
drawLastRow :: Float -> Float -> [Cell] -> [Picture]
drawLastRow _ _ []                 =      [Blank]
drawLastRow pos_x pos_y l          =      (drawLastCell 
                                                         pos_x 
                                                         pos_y $ 
                                                         head l) 
                                                     ++ (drawLastRow (pos_x + sizeCell)   pos_y  $ tail l)

--отрисовка фишки последней строки (не рисуе квадратик)
drawLastCell :: Float -> Float -> Cell -> [Picture]
drawLastCell _ _ Nothing                  =       [Blank]
drawLastCell pos_x pos_y (Just x)         =       [Translate 
                                                         pos_x 
                                                         pos_y $ 
                                                         Color (col x) $ circleSolid (sizeCell / 2)]
                                          where
                                          col Black = black
                                          col Red = red

--обработка внешних событий
handle :: Event -> World -> World
handle (EventKey (Char 'z') Down _ _) (World a b c d e f Empty (x,y,z,_))= World a b c d e f (Main 0) (x,y,z,True)
handle (EventMotion (x,y)) (World a b c d e f (Main g) (x1,y1,z1,_))     = handle_menu Move (x,y) (World a b c d e f (Main g) (x1,y1,z1,True)) 
handle (EventKey (MouseButton LeftButton) Down _ (x,y)) (World a b c d e f (Main g) (x1,y1,z1,_)) = handle_menu Click (x,y) (World a b c d e f (Main g) (x1,y1,z1,True))
handle (EventKey (MouseButton LeftButton) Down _ (x,y)) (World a b c d e f Opt (x1,y1,z1,_)) = handle_menu Click (x,y) (World a b c d e f Opt (x1,y1,z1,True))

handle _ (World a b c d e f g (x,y,z,True)) = (World a b c d e f g (x,y,z,True))
handle (EventKey (SpecialKey KeySpace) Down _ _) w =  getback w
handle       _                  (World m s (W Red) p b t menu (x,y,z,v)) = World m s (W Red) p b t menu (x,y,z,v)
handle       _                  (World m s (W Black) p b t menu (x,y,z,v)) = World m s (W Black) p b t menu (x,y,z,v)
handle (EventKey (MouseButton LeftButton) Down _ (x,y)) w = checkWorld (mainNumberRow (x,y),mainNumberCol (x,y)) w
handle _ w = w

--обрабоктка событий в меню 
handle_menu :: MouseEvent -> Types.Point -> World -> World
handle_menu event (x,y) (World a b c d e f (Main g) (x1,y1,z1,p)) | t (-120) 120 (-10) 30
                                                       = case event of
                                                         Move -> menu 1
                                                         Click-> loadGame (menu g)
                                                     | t (-120) 120 (-70) (-30)
                                                       = case event of
                                                         Move -> menu 2
                                                         Click-> menu 2
                                                     | t (-120) 120 (-130) (-90)
                                                       = case event of
                                                         Move -> menu 3
                                                         Click-> (World a b c d e f Opt (x1,y1,z1,p))
                                                     |t 150 190 90 120
                                                       = case event of
                                                         Click->  World a b c d e f Empty (x1,y1,z1,False)
                                                         Move ->  menu g 
                                                     | otherwise = menu g 
                                                     where 
                                                     t dx1 dx2 dy1 dy2 = (x >= (offsetX + dx1) && x <= (offsetX + dx2)) && (y >= dy1 && y <= dy2)
                                                     menu n = World a b c d e f (Main n) (x1,y1,z1,p) 
                                                     
                                                     
handle_menu Click (x,y) (World a b c d e f Opt (x1,y1,z1,p))| t 150 190 90 120
                                                         = World a b c d e f Empty (x1,y1,z1,False)
                                                            | t (-180) (-60) 10 60
                                                         = case x1 of
                                                                Limit    -> World a b c d e f Opt (No_limit,y1,z1,p)
                                                                No_limit -> World a b c d e (20,20) Opt (Limit,y1,z1,p) 
                                                            | t (-180) (-20) (-70) (-20)
                                                         = case y1 of
                                                                Hard -> World a b c d e f Opt (x1,Easy,z1,p)
                                                                Easy -> World a b c d e f Opt (x1, Hard,z1,p)                                                              
                                                            | t (-180) (-80) (-150) (-100)
                                                         = case z1 of 
                                                                Hum_Comp -> World a b c d e f Opt (x1,y1,Hum_Hum,p)
                                                                Hum_Hum  -> World a b c d e f Opt (x1,y1,Hum_Comp,p)
                                                            | t (-180) (-130) (-200) (-160)
                                                         = World a b c d e f (Main 0) (x1,y1,z1,p) 
                                                            | otherwise = World a b c d e f Opt (x1,y1,z1,p)
                                                             where 
                                                             t dx1 dx2 dy1 dy2 = (x >= (offsetX + dx1) && x <= (offsetX + dx2)) && (y >= dy1 && y <= dy2)






