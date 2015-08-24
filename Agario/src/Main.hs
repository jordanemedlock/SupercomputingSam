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
startCellMass = floor ((fromIntegral width+height :: Double)/4)
maxFareMass = startCellMass `div` 2

main :: IO ()
main = do
  fare <- take 100 <$> generateRandomPopulation width height maxFareMass
  let winConfig   = WindowConfig { wcPosition = Point2D 558 16
                                 , wcSize = Point2D width height
                                 , wcName = "Agar.IO"
                                 }
  let tile        = (0, False, 0, ())
  let tiles       = replicate 10 $ replicate 10 tile
  let bmpList     = [("tex.bmp", Nothing)]
  let gameMap     = tileMap tiles 40 40
  let cell        = Cell {name="hard coded", cellNum = 0, mass = startCellMass, pos = (0,0), rgb = (1,0,0)}
  let playerGroup = objectGroup cellManagerName [createCell cell]
  let fareGroup   = objectGroup "Fares" $ createFares fare
  let objManagers = [fareGroup, playerGroup]
  let inputs      = [(MouseButton LeftButton, StillDown, mouseHandler)
                    ,(Char '\27', Press, \_ _ -> funExit)
                    ]
  funInit winConfig gameMap objManagers 0 0 inputs stepAction (Timer 30) bmpList


mouseHandler :: Modifiers -> Position -> IOGame t s u v ()
mouseHandler _ (Position x y) = do
  gameObject <- findObject "player" cellManagerName
  (Point2D xPos yPos) <- getObjectPosition gameObject
  let (w,h) = (width :: GLdouble, height :: GLdouble)
  setObjectSpeed (Point2D (fromIntegral (x - floor xPos) * 0.01) (fromIntegral ((floor (w/2) - y) - (floor yPos - floor (h/2))) * 0.01)) gameObject
  -- liftIOtoIOGame $ do
  --   print $ "(curX, curY): " ++ show (x,y)
  --   print $ "(objX, objY): " ++ show (xPos, yPos)
  return ()


stepAction :: IOGame t Cell u v ()
stepAction = do
  playerObj <- findObject "player" cellManagerName
  fareObjs <- getObjectsFromGroup "Fares"
  col <- objectListObjectCollision fareObjs playerObj

  when col $
    do collidee <- getFareFromCollision fareObjs playerObj
       setObjectAsleep True collidee
       let cell = getGameObjectAttribute playerObj
       let fare = getGameObjectAttribute collidee
       let newCell = combineMasses cell fare
       let player = updateObjectAttribute newCell playerObj
       let player' = updateObjectSize (Point2D (realToFrac $ radius newCell) (realToFrac $ radius newCell)) player

       objMans <- getObjectManagers

       let newFObjMan = destroyGameObject (getGameObjectName collidee) "Fares" objMans
       let newPObjMan = updateObject (const player') (getGameObjectId playerObj) cellManagerName objMans
       let fareObjMan = searchObjectManager "Fares" newFObjMan
       let playerObjMan = searchObjectManager cellManagerName newPObjMan
       

       setObjectManagers [fareObjMan, playerObjMan]
  updateObjectsPostitions
  moveAllObjects
  return ()

updateCell :: GameObject Cell -> IOGame t Cell u v (GameObject Cell)
updateCell cellObject = do
    let cell = getGameObjectAttribute cellObject
    cellPos <- getObjectPosition cellObject
    let newCell = cell { pos = (realToFrac *** realToFrac) (xPos cellPos, yPos cellPos) }
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

createCell :: Cell -> GameObject Cell
createCell cell =
  let cellPic = createCellPicture (radius cell) (rgb cell)
  in object "player" cellPic False (Point2D (width/2) (height/2)) (Point2D 0 0) cell

createFare :: (Double -> (Double, Double, Double) -> ObjectPicture) -> Cell -> GameObject Cell
createFare picture fare = object (show $ cellNum fare) (picture (radius fare) (rgb fare)) False (Point2D (realToFrac $ fst $ pos fare) (realToFrac $ snd $ pos fare)) origin fare

createFares :: Cells -> [GameObject Cell]
createFares = map (createFare createCellPicture)

getFareFromCollision :: [GameObject Cell] -> GameObject Cell -> IOGame t Cell u v (GameObject Cell)
getFareFromCollision [] cell = return cell
getFareFromCollision (fare:fares) cell = do
  col <- objectsCollision fare cell
  if col
    then return fare
    else getFareFromCollision fares cell
