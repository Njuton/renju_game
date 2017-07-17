module Main where

import Graphics
import Types
import Files
import Graphics.Gloss

main :: IO ()
main    
 = do
   rejnzu     <- loadBMP "img/rejnzu.bmp"  --0
   red_win    <- loadBMP "img/red_win.bmp" --1
   black_win  <- loadBMP "img/black_win.bmp"   --2
   tie        <- loadBMP "img/tie.bmp"         --3
   play_game  <- loadBMP "img/play_game.bmp"   --4
   texture    <- loadBMP "img/texture.bmp"     --5
   timer_b    <- loadBMP "img/b.bmp"           --6
   timer_r    <- loadBMP "img/r.bmp"           --7
   back       <- loadBMP "img/bmp/back.bmp"    --8
   button     <- loadBMP "img/bmp/button.bmp"  --9
   fon        <- loadBMP "img/bmp/fon.bmp"    --10
   hard       <- loadBMP "img/bmp/hard.bmp"   --11
   hardness   <- loadBMP "img/bmp/hardness.bmp"--12
   load       <- loadBMP "img/bmp/load.bmp"   --13
   menu       <- loadBMP "img/bmp/menu.bmp"   --14
   mode       <- loadBMP "img/bmp/mode.bmp"   --15
   ok         <- loadBMP "img/bmp/ok.bmp"     --16
   options    <- loadBMP "img/bmp/options.bmp"--17
   options_1  <- loadBMP "img/bmp/options_1.bmp"--18
   save       <- loadBMP "img/bmp/save.bmp"     --19
   time       <- loadBMP "img/bmp/time.bmp"     --20
   time_2     <- loadBMP "img/bmp/time_2.bmp"   --21
   time_text  <- loadBMP "img/bmp/time_text.bmp"--22
   x          <- loadBMP "img/bmp/x.bmp"        --23
   easy       <- loadBMP "img/bmp/easy.bmp"     --24
   h_h        <- loadBMP "img/bmp/h.bmp"        --25
   h_c        <- loadBMP "img/bmp/h_c.bmp"      --26
   
             
   go (World (matrixFiling sizeField) Black None [rejnzu,red_win,black_win,tie,play_game,texture,timer_b,timer_r,back,button,fon,hard,hardness,load,menu,mode,ok,options,options_1,save,time,time_2,time_text,x, easy,h_h,h_c] Nothing (20,20) Empty (No_limit,Easy,Hum_Hum,False))            