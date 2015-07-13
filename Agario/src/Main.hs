module Main where

import Graphics.UI.Fungen
import Types.Cells hiding (main)
import Graphics.Rendering.OpenGL (GLdouble)

width = 400  
height = 400
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble

mouseHandler _ (Position x y) = do
  gameObject <- findObject "player" "Cells"
  (xPos, yPos) <- getObjectPosition gameObject
  setObjectSpeed (fromIntegral (x - floor xPos) * 0.01, fromIntegral ((floor (w/2) - y) - (floor yPos - floor (h/2))) * 0.01) gameObject
  liftIOtoIOGame $ do
    print $ "(curX,curY): " ++ show (x,y)
    print $ "(objX, objY): " ++ show (xPos, yPos)
  return ()

stepAction = do
  moveAllObjects
  return ()

main = do
  let winConfig = ((558,116),(width,height), "Agar.IO")
      map = colorMap 0.7 0.7 0.7  w h
      cellPic = Basic $ Circle 10 1 0 0 Filled  
      cell = Cell {name="hard coded", cellNum = 0, mass = 100, pos = (0,0)}
      player = object "player" cellPic False (w/2, h/2) (0,0) cell 
      cells = objectGroup "Cells" [player]
      inputHandlers = [ (MouseButton LeftButton, Press, mouseHandler)
                       ,(Char '\27', Press, \_ _ -> funExit)
                      ]

  funInit winConfig map [cells] 0 0 inputHandlers stepAction (Timer 30) [] 
