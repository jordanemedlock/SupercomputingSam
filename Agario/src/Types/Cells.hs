module Types.Cells where

import qualified Data.List as L
import qualified Data.List.Split as S
import System.Random
import Control.Applicative

type Position = (Double, Double)

type Cells = [Cell]

data Cell = Cell { name :: String
                 , alive :: Bool
                 , cellNum :: Int
                 , mass :: Int
                 , pos :: Position 
                 } deriving (Show)

instance Ord Cell where
  a <= b = mass a <= mass b

instance Eq Cell where
  a == b = cellNum a == cellNum b

-- | Finds the radius of a circular Cell using its mass as the Area by finding
-- | the square root of the area over pi 
radius :: Cell -- ^ contains the data of a cell to find the
       -> Double -- ^ radius as a double
radius = sqrt . (/pi) . fromIntegral . mass 


-- | Calculates the distance between two points by taking taking the square   
-- | root of the the sum of the change inthe x coordinate squared and the 
-- | change in the y coordinate squared
distance :: Position -- ^ position type with (x,y) as two doubles 
         -> Position 
         -> Double -- ^ distance between points as a double
distance (x,y) (x2,y2) = sqrt((x2-x)**2 + (y2-y)**2)

-- | Uses the "distance" function to find the possiton between two cells
distanceBtwnCells :: Cell -- ^ uses the positon in the cell's data 
                  -> Cell 
                  -> Double -- ^ distance between two cells as a double
distanceBtwnCells cell1 cell2 = pos cell1 `distance` pos cell2 

-- | Tests if two cells are intersecting by finding the distance between the
-- | cells and testing if that is less than the sum of their radii
intersect :: Cell -- ^ uses cell's position and mass data 
          -> Cell 
          -> Bool -- ^ intersection as a True or False value 
intersect cell1 cell2 = cell1 `distanceBtwnCells` cell2 < radius cell1 + radius cell2 

-- Finds the x or the y coordinate of every cell in a list
getX, getY :: Cells 
           -> [Double] -- ^ x or y coordinate as a list of doubles
getX = map $ fst . pos
getY = map $ snd . pos

-- | Finds the x and the y cordinate of every cell in a list
getXY :: Cells 
      -> [Position] -- ^ x and y as a list of the "Positiion" type
getXY = map pos  

-- | Finds and returns a cell with the mass between two cells
greaterMass :: Cell -- ^ uses the mass of two cells
            -> Cell 
            -> Cell -- ^ larger cell as a cell
greaterMass = max

-- | Sees if two cells are intersecting or not, and if they are, then makes the 
-- | smaller cell Nothing and gives the larger cell the smaller's mass
absorb :: Cell -- ^ uses cells mass, and the "intersect" function 
       -> Cell 
       -> (Cell, Cell) -- ^ absorbed cell as a tuple with Just cell or Nothing
absorb cell1 cell2 
    | inters && mass cell1 > mass cell2 = (cell1 {mass = newMass}, cell2 {alive = False})
        -- returns Just cell1 with the newMass when cell1 is larger and they intersect 
    | inters && mass cell1 < mass cell2 = (cell1 {alive = False}, cell2 {mass = newMass})
        -- Does the same as above except using cell2 instead 
    | otherwise                         = (cell1, cell2)
        -- returns the two cells as Just cells when they don't intersect
    where inters = cell1 `intersect` cell2
          newMass = mass cell1 + mass cell2  

-- | Uses the position of a mouse cursor to move the cell  
move :: Position -- ^ Position of the mouse cursor
     -> Cell -- ^ uses position of the cell
     -> Cell -- ^ creates a new cell with a position moving towards the cursor 
move cursor@(curX, curY) cell = cell { pos = (x + dx, y + dy) }
    -- finds the point the cell need to move trough crazy trigonometry
  where l = cursor `distance` pos cell
        progress = if l > threshold then upperSpeed else l*lowerSpeedScale
        (x,y) = pos cell
        theta = atan ((y - curY) / (x - curX))
        dx = cos theta * progress
        dy = sin theta * progress
        threshold = 10
        upperSpeed = 10
        lowerSpeedScale = 0.1

idEquals :: Cell -> Cell -> Bool
idEquals cell1 cell2 = cellNum cell1 == cellNum cell2 

intersections :: Cells -> [(Cell, Cell)]
intersections cells = [(x,y) | x <- cells, y <- cells, x `intersect` y, not (x `idEquals` y)]

elemBy :: (Eq b) => (a -> b) -> a -> [a] -> Bool
elemBy f x xs = (not.null) $ filter ((f x == ) . f) xs 

cellNumElem :: Cell -> Cells -> Bool
cellNumElem = elemBy cellNum

combineMasses :: Cell -> Cell -> Cell
combineMasses c1 c2 = c1 { mass = mass c1 + mass c2 }

checkCollisions :: Cells -> Cells
checkCollisions cells = filter (not.(`cellNumElem` (alive++dead))) cells ++alive
  where (alive,dead) = helper (intersections cells) [] []
        helper [] alive dead = (alive, dead)
        helper ((x,y):xs) alive dead
          | x > y && not (x `cellNumElem` alive) = helper xs (combineMasses x y:alive) (y:dead)
          | y > x && not (y `cellNumElem` alive) = helper xs (combineMasses y x:alive) (x:dead)
          | not (x `cellNumElem` alive 
              || y `cellNumElem` alive)          = helper xs (x:y:alive) dead
          | otherwise                            = helper xs alive dead



-- This bastard works if looped multiple times... I could prob make this work
-- with recursion, but ima just say no to that. Don't judge me, I'm in Mexico, damn it!
--checkCollisions :: Cells -> Cells
--checkCollisions cs = L.nubBy idEquals $ L.sort $ concat [tupleToList $ absorb a b | a <- cs, b <- cs, not $ a `idEquals` b]

tupleToList :: (Cell, Cell) -> Cells
tupleToList (cell1, cell2)
    | alive cell1 && not (alive cell2) = [cell1]
    | not (alive cell1) && alive cell2 = [cell2]
    | otherwise                        = [cell1, cell2]

generateRandomPopulation :: IO Cells
generateRandomPopulation = do
  gen1 <- newStdGen
  let names = S.chunksOf 5 $ randomRs ('a','z') gen1
  let alives = repeat True
  let cellNums = [0..]
  gen2 <- newStdGen
  let masses = randomRs (0,100) gen2
  gen3 <- newStdGen
  let xs = randomRs (0.0,100.0) gen3  
  gen4 <- newStdGen
  let ys = randomRs (0.0,100.0) gen4 
  let positions = zip xs ys
  return $ L.zipWith5 Cell names alives cellNums masses positions 


main :: IO()
main = do
  let cell1 = Cell { name = "Hugh G. Rection" 
                   , alive = True
                   , cellNum = 0
                   , mass = 100
                   , pos = (7.426019951630333,10) 
                   }
  let cell2 = Cell { name = "Not Hugh G. Rection"
                   , alive = True
                   , cellNum = 0
                   , mass = 10
                   , pos = (0,10) 
                   }
  let inter = cell1 `intersect` cell2
  cells <- take 10 <$> generateRandomPopulation
  print $ intersections cells
  putStrLn ""
  print $ checkCollisions cells


-- Heres the FunGEn URLs
-- https://github.com/simonmichael/fungen/blob/master/examples/pong/pong.hs
-- http://hackage.haskell.org/package/FunGEn-0.4.6.1/docs/Graphics-UI-Fungen.html