module Main where

import Graphics.UI.Fungen
import Control.Applicative
import Types.Cells hiding (main)
import Types.Fare hiding (main)
import Graphics.Rendering.OpenGL (GLdouble)
import Control.Arrow ((***))

width, height :: (Num a) => a
width = 400
height = 400
objectManagerName = "Cells"

enum :: (Enum a, Enum b) => a -> b
enum = toEnum . fromEnum

mouseHandler _ (Position x y) = do
  gameObject <- findObject "player" objectManagerName
  (xPos, yPos) <- getObjectPosition gameObject
  let (w,h) = (width :: GLdouble, height :: GLdouble)
  setObjectSpeed (fromIntegral (x - floor xPos) * 0.01, fromIntegral ((floor (w/2) - y) - (floor yPos - floor (h/2))) * 0.01) gameObject
  liftIOtoIOGame $ do
    print $ "(curX,curY): " ++ show (x,y)
    print $ "(objX, objY): " ++ show (xPos, yPos)
  return ()

stepAction = do
  moveAllObjects
  return ()

updateCell cellObject = do
    let cell = getGameObjectAttribute cellObject
    cellPos <- getObjectPosition cellObject
    let newCell = cell { pos = (realToFrac *** realToFrac) cellPos }
    return $ updateObjectAttribute newCell cellObject

updateObjectsPostitions = do
  cells <- getObjectsFromGroup objectManagerName
  newCells <- mapM updateCell cells
  let group = objectGroup objectManagerName newCells
  setObjectManagers [group]

main = do
  let winConfig = ((558,116),(width,height), "Agar.IO")
      gameMap = colorMap 0.7 0.7 0.7  width height
      cellPic r = Basic $ Circle (realToFrac r) 1 0 0 Filled
      cell = Cell {name="hard coded", cellNum = 0, mass = 100, pos = (0,0)}
      player = object "player" (cellPic $ radius cell) False (width/2, height/2) (0,0) cell
      cells = objectGroup objectManagerName [player]
      inputHandlers = [ (MouseButton LeftButton, StillDown, mouseHandler)
                      ,(Char '\27', Press, \_ _ -> funExit)
                      ]
  fare <- take 100 <$> generateRandomPopulation width height
  let fareObjects = map (fareToObject cellPic) fare
      fares = objectGroup "fares" fareObjects
  funInit winConfig gameMap [cells, fares] 0 0 inputHandlers stepAction (Timer 30) []

fareToObject :: (Double -> ObjectPicture) -> Cell -> GameObject Cell
fareToObject picture fare = object "fare" (picture $ radius fare) False ((realToFrac *** realToFrac) (pos fare)) (0,0) fare
