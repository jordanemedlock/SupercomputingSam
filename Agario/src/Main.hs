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
cellManagerName = "Cells"
startCellMass = enum ((fromIntegral width+height :: Double)/8) :: Int
maxFareMass = startCellMass `div` 2

main = do
  let winConfig = ((558,116),(width,height), "Agar.IO")
  let gameMap = colorMap 0.7 0.7 0.7  width height
  let cellPic radius r g b = Basic $ Circle (realToFrac radius) (realToFrac r) (realToFrac g) (realToFrac b) Filled
  let cell = Cell {name="hard coded", cellNum = 0, mass = startCellMass, pos = (0,0), rgb = (1,0,0)}
  let player = object "player" (cellPic (radius cell) r g b) False (width/2, height/2) (0,0) cell
          where r = fst' $ rgb cell
                g = snd' $ rgb cell
                b = trd  $ rgb cell
  let cells = objectGroup cellManagerName [player]
  let inputHandlers = [ (MouseButton LeftButton, StillDown, mouseHandler)
                      ,(Char '\27', Press, \_ _ -> funExit)
                      ]
  fare <- take 100 <$> generateRandomPopulation width height maxFareMass
  print $ length fare
  let fareObjects = map (fareToObject cellPic) fare
  let fares = objectGroup "Fares" fareObjects
  funInit winConfig gameMap [fares, cells] 0 0 inputHandlers stepAction (Timer 30) []


mouseHandler _ (Position x y) = do
  gameObject <- findObject "player" cellManagerName
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
  cells <- getObjectsFromGroup cellManagerName
  newCells <- mapM updateCell cells
  let group = objectGroup cellManagerName newCells
  setObjectManagers [group]

enum :: (Enum a, Enum b) => a -> b
enum = toEnum . fromEnum

fst' :: (a,b,c) -> a
fst' (x,_,_) = x
snd' :: (a,b,c) -> b
snd' (_,y,_) = y
trd :: (a,b,c) -> c
trd (_, _, z) = z

fareToObject :: (Double -> Double -> Double -> Double -> ObjectPicture) -> Cell -> GameObject Cell
fareToObject picture fare = object "fare" (picture (radius fare) r g b) False ((realToFrac *** realToFrac) (pos fare)) (0,0) fare
    where r = fst' $ rgb fare
          g = snd' $ rgb fare
          b = trd  $ rgb fare
