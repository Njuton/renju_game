module Logic where

import Types
import Data.Matrix

--реалищация функции "отмена хода"
getback :: World -> World
getback (World m s w p Nothing t men x) = (World m s w p Nothing t men x)
getback (World _ _ _ _ (Just b) t _ _) = b 

--обрабочик мира
checkWorld :: PointI -> World -> World
checkWorld (0,_) m = m
checkWorld (_,0) m = m
checkWorld coord (World m s l p b t menu (x,y,z,ps)) | m ! coord == Nothing  =  World
                                                           (putIn coord s m)
                                                           (inverseState s)
                                                            (gameRules coord s (putIn coord s m))
                                                            p
                                                            (Just (World m s l p b t menu (x,y,z,ps)))
                                                            t
                                                            menu
                                                            (x,y,z,ps)
                                     | otherwise             =  World m s l p b t menu (x,y,z,ps)

--получение номера столбца
mainNumberCol :: Point -> Int
mainNumberCol x                          = numberCol
                                           x
                                           (offsetX - sizeCell - fromIntegral((sizeField - 1) `div` 2) * sizeCell) 
                         

numberCol :: Point -> Float -> Int
numberCol (x,_) n | x < n || x > (n + fromIntegral(sizeField) * sizeCell) = 0
                  | otherwise                                             = div sizeField 2 + 1 + div (round (x - offsetX)) (round sizeCell)

--получение номера строки
mainNumberRow :: Point -> Int
mainNumberRow x = numberRow
                          x
                         (offsetY + sizeCell + fromIntegral((sizeField - 1) `div` 2) * sizeCell) 
                         

numberRow :: Point -> Float -> Int
numberRow (_,y) n | y > n || y < (n - fromIntegral(sizeField) * sizeCell) = 0
                  | otherwise                                             = div sizeField 2 - div (round ( y - offsetY)) (round sizeCell)

--заполнение ячейки матрицы
putIn :: PointI -> State -> Matrix Cell -> Matrix Cell
putIn (a,b) Black m = setElem (Just Black) (a,b) m
putIn (a,b) Red m   = setElem (Just Red) (a,b) m 

--инвертирование состояния
inverseState :: State -> State
inverseState Black = Red
inverseState Red = Black

--получение окрестности 5 для строки
getNeighRow :: PointI -> Matrix Cell -> [Cell]
getNeighRow (a,b) m = [m ! (i,j) | i <- [a]
                          , j <- [(b - 4) .. (b + 4)]
                          , j >= 1 && j <= (ncols m)]

--получение окрестности 5 для столбца
getNeighCol :: PointI -> Matrix Cell -> [Cell]
getNeighCol (a,b) m = [m ! (i,j) | i <- [(a - 4) .. (a + 4)]
                          , j <- [b]
                          , i >= 1 && i <= (nrows m)]

--получение окрестности 5 по диагонали влево

getNeighDiag :: PointI -> Matrix Cell -> Diagonal -> [Cell]
getNeighDiag (a,b) m L = diag (a,b) m (a + 4) (b + 4) L
getNeighDiag (a,b) m R = diag (a,b) m (a + 4) (b - 4) R


diag :: PointI -> Matrix Cell -> Int -> Int -> Diagonal -> [Cell]
diag (a,b) m i j d
                     | i == (a - 5) = []
                     | i >= 1 && i <= (nrows m) && j>=1 && j <= (ncols m) = m ! (i,j) : (diag (a,b) m (i - 1) (lr d) d)
                     | otherwise = diag (a,b) m (i - 1) (lr d) d
                     where
                     lr L = j - 1
                     lr R = j + 1

--по [Cell] определяет, есть ли выигрыш

winner :: [Cell] -> Bool
winner l = winFunc l 0 

winFunc :: [Cell] -> Int -> Bool
winFunc _ 4 = True
winFunc (x : (y : xs)) ac | x == y && (x /= Nothing) = winFunc (y : xs) (ac + 1) 
                          | otherwise = winFunc (y:xs) 0 
winFunc _ _ = False

--Определяет, есть ли выигрыш в игре
--None - никто, Tie - полностью заполнена, иначе победитель (W Black |W Red)

gameRules :: PointI -> State -> Matrix Cell ->Win
gameRules (x,y) s m
                  | winner ( getNeighDiag (x,y) m L) || 
                      winner ( getNeighDiag (x,y) m R) || 
                      winner ( getNeighRow (x,y) m ) ||
                      winner ( getNeighCol (x,y) m ) 
                    = (W s)
                  | fullBoard (toList m) = Tie
                  | otherwise = None

--заполнена ли доска

fullBoard :: [Cell] -> Bool
fullBoard [] = True
fullBoard (Nothing : _) = False
fullBoard l = fullBoard (tail l)
