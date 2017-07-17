module Types where

import Data.Matrix
import Graphics.Gloss

data State = Black | Red
                     deriving (Show, Eq)
data W a = W a | Tie | None

data Diagonal = L | R

data Hard = Easy | Hard
data Mode = Hum_Comp | Hum_Hum
data Time = Limit | No_limit
type Pause = Bool

data MouseEvent = Click | Move

data Menu = Main {anum :: Int} | Opt | Empty

type Win = W State
               
type Cell = Maybe State 

type Field = Matrix Cell
type PointI = (Int,Int)
type Point = (Float,Float)

data World = World
          { field:: Field                 -- матрица значений
          , state:: State                 -- чей ход
          , win  :: Win                   -- флаг конца игры
          , pic  :: [Picture]             -- загруженные изображения
          , back :: Maybe World           -- отмена хода (прошлый мир)
          , timer:: PointI                -- таймер для обоих игроков 
          , menu :: Menu                  -- объекты меню
          , mode :: (Time,Hard,Mode,Pause)--состояния игры (Ограничение по времени, сложность, Игрок-ПК/Игрок, Пауза)
          }

--размер ячейки
sizeCell :: Float
sizeCell = 40.0

--размерность матрицы (размерность сетки + 1) 
sizeField :: Int
sizeField = 10

--положение центра игровой доски
offsetX :: Float
offsetX = 0

offsetY :: Float
offsetY = (-50)

