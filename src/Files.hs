module Files where

import Types
import Data.Matrix
import System.IO.Unsafe
import Prelude hiding (catch)
import Control.Exception
import System.Environment
import System.IO.Error (isDoesNotExistError)

--загрузка игры из файла - главная функция
loadGame :: World -> World
loadGame (World _ _ c d e _ _ h)= World field state c d e (time1,time2) (Main 1) h
         where
         spisok = unsafePerformIO (loadFile `catch` umolchanie)
         field = fst(spisok)
         state = fst(snd(spisok))
         time1 = read(fst(snd(snd(spisok))))
         time2 = read(snd(snd(snd(spisok))))
         
--обработка исключений (не открылся файл)
umolchanie :: IOException -> IO (Matrix Cell,(State,(String,String)))
umolchanie _ = return ((matrixFiling sizeField),(Black,("0","0")))

--загрузка из файла
loadFile :: IO (Matrix Cell,(State,(String,String)))
loadFile = do
           file1 <- readFile "save/save_w.txt"
           file2 <- readFile "save/save_st.txt"
           return (rec_matrix 1 1 file1 $ matrixFiling sizeField, rec_state file2)

--загрузка матрицы (Строка, Столбец) file Rezult_Matrix 
rec_matrix :: Int -> Int -> String -> Matrix Cell -> Matrix Cell
rec_matrix _ _ [] m = m
rec_matrix i j (x:xs) m             | x == '\n' = rec_matrix (i + 1) 1 xs m
                                    | x == ' '  = rec_matrix i j xs m 
                                    | otherwise = rec_matrix i (j + 1) xs (setElem (f x) (i,j) m)
                                    where
                                    f '0' = Nothing
                                    f '1' = (Just Black)
                                    f '2' = (Just Red)
--загрузка того, кто ходит и времени
rec_state :: String -> (State,(String,String))
rec_state (x:xs)   | x == 'B' = (Black, rec_time (tail xs) [] ("",""))
                   | x == 'R' = (Red, rec_time (tail xs) [] ("",""))
                   
rec_time :: String -> String -> (String,String) -> (String,String)
rec_time [] _ s = s
rec_time (x:xs) buf (a,b)   | x == '\n' = (a,reverse buf)
                            | x == ' '  = rec_time xs [] (reverse buf,"")
                            | otherwise = rec_time xs (x:buf) (a,b)         

--заполнение матрицы разерности n
matrixFiling :: Int -> Matrix Cell
matrixFiling n = matrix n n $ \ _ -> Nothing