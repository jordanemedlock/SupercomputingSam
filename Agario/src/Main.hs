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
startCellMass = floor ((fromIntegral width+height :: Double)/8)
maxFareMass = startCellMass `div` 2

main :: IO ()
main = do
  fare <- take 100 <$> generateRandomPopulation width height maxFareMass
  let winConfig   = ((558,116),(width,height), "Agar.IO")
  let tile        = (0, False, 0, ())
  let tiles       = replicate 10 $ replicate 10 tile
  let bmpList     = [("tex.bmp", Nothing)]
  let gameMap     = tileMap tiles 40 40
  let cell        = Cell {name="hard coded", cellNum = 0, mass = startCellMass, pos = (0,0), rgb = (1,0,0)}
  let cells       = objectGroup cellManagerName [createCell cell width height]
  let fares       = objectGroup "Fares" $ createFares fare
  let objManagers = [fares, cells]
  let inputs      = [(MouseButton LeftButton, StillDown, mouseHandler)
                    ,(Char '\27', Press, \_ _ -> funExit)
                    ]
  funInit winConfig gameMap [fares, cells] 0 0 inputs stepAction (Timer 30) bmpList


mouseHandler :: Modifiers -> Position -> IOGame t s u v ()
mouseHandler _ (Position x y) = do
  gameObject <- findObject "player" cellManagerName
  (xPos, yPos) <- getObjectPosition gameObject
  let (w,h) = (width :: GLdouble, height :: GLdouble)
  setObjectSpeed (fromIntegral (x - floor xPos) * 0.01, fromIntegral ((floor (w/2) - y) - (floor yPos - floor (h/2))) * 0.01) gameObject
  -- liftIOtoIOGame $ do
  --   print $ "(curX, curY): " ++ show (x,y)
  --   print $ "(objX, objY): " ++ show (xPos, yPos)

  return ()

stepAction :: IOGame t Cell u v ()
stepAction = do
  
  playerObj <- findObject "player" cellManagerName 
  fareObjs  <- getObjectsFromGroup "Fares" 
  col       <- objectListObjectCollision fareObjs playerObj

  if col 
    then (do letsDoATest <- getFareFromCollision fareObjs playerObj
             setObjectAsleep True letsDoATest
             printOnPrompt "YAY!!!!")
    else printOnPrompt "Nothing"

  -- printOnPrompt $ length fareObjs

  updateObjectsPostitions
  moveAllObjects
  return ()

updateCell :: GameObject Cell -> IOGame t Cell u v (GameObject Cell)
updateCell cellObject = do
    let cell = getGameObjectAttribute cellObject
    cellPos <- getObjectPosition cellObject
    let newCell = cell { pos = (realToFrac *** realToFrac) cellPos }
    return $ updateObjectAttribute newCell cellObject

updateObjectsPostitions :: IOGame t Cell u v ()
updateObjectsPostitions = do
  fareGroup <- findObjectManager "Fares"
  cells     <- getObjectsFromGroup cellManagerName
  newCells  <- mapM updateCell cells
  let cellGroup = objectGroup cellManagerName newCells
  setObjectManagers [fareGroup, cellGroup]

createCellPicture :: Double -> (Double, Double, Double) -> ObjectPicture
createCellPicture r' (r, g, b) = Basic $ Circle radius red green blue Filled
    where radius = realToFrac r'
          red    = realToFrac r
          green  = realToFrac g
          blue   = realToFrac b

createCell :: Cell -> GLdouble -> GLdouble -> GameObject Cell
createCell c w h = 
  let cellPic = createCellPicture (radius c) (rgb c)
  in object "player" cellPic False (w/2, h/2) (0,0) c 

fareToObject :: (Double -> (Double, Double, Double) -> ObjectPicture) -> Cell -> GameObject Cell
fareToObject picture fare = object (show $ cellNum fare) (picture (radius fare) (rgb fare)) False ((realToFrac *** realToFrac) (pos fare)) (0,0) fare

createFares :: Cells -> [GameObject Cell] 
createFares = map (fareToObject createCellPicture) 

getFareFromCollision :: [GameObject Cell] -> GameObject Cell -> IOGame t Cell u v (GameObject Cell)
getFareFromCollision [] cell = return cell
getFareFromCollision (fare:fares) cell = do
  col <- objectsCollision fare cell
  if col 
    then return fare 
    else getFareFromCollision fares cell
