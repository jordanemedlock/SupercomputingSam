module Main where

import Graphics.UI.Fungen
import Types.Cells hiding (main)


mouseHandler _ (Position x y) = do

  gameObject <- findObject "player" "Cells"

  (xPos, yPos) <- getObjectPosition gameObject

  setObjectSpeed (fromIntegral (x - floor xPos) * 0.01, fromIntegral ((500 - y) - (floor yPos - 500)) * 0.01) gameObject

  liftIOtoIOGame $ do
    print $ "(x,y): " ++ show (x,y)
    print $ "(xPos, yPos): " ++ show (xPos, yPos)

  return ()

stepAction = do
  moveAllObjects
  return ()

main = do
  let winConfig = ((558,116),(1000,1000), "Agar.IO")
  let map = colorMap 0.5 0.5 0.5 1000 1000
  let cellPic = Basic $ Circle 10 1 0 0 Filled  
  let cell = Cell {name="hard coded", cellNum = 0, mass = 100, pos = (0,0)}
  let player = object "player" cellPic False (500,500) (0,0) cell 
  let cells = objectGroup "Cells" [player]

  let inputHandlers = [ (MouseButton LeftButton, Press, mouseHandler)
                      , (Char '\27', Press, const (const funExit))
                      ]

  funInit winConfig map [cells] 0 0 inputHandlers stepAction Idle [] 