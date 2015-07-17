module Types.Cells where

import qualified Data.List as L

type Pos = (Double, Double)

type Cells = [Cell]

data Cell = Cell { name :: String
                 , cellNum :: Double
                 , mass :: Int
                 , pos :: Pos
                 , rgb :: (Double, Double, Double) 
                 } deriving (Show)

instance Ord Cell where
  a <= b = mass a <= mass b

instance Eq Cell where
  a == b = cellNum a == cellNum b

-- | Finds the radius of a circular Cell using its mass as the Area by finding
-- | the square root of the area over pi 
radius :: Cell -- ^ contains the data of a cell to find the
       -> Double -- ^ radius as a double
radius = sqrt.(/pi).fromIntegral.mass 


-- | Calculates the distance between two points by taking taking the square   
-- | root of the the sum of the change inthe x coordinate squared and the 
-- | change in the y coordinate squared
distance :: Pos -- ^ Pos type with (x,y) as two doubles 
         -> Pos 
         -> Double -- ^ distance between points as a double
distance (x,y) (x2,y2) = sqrt((x2-x)**2 + (y2-y)**2)

-- | Uses the "distance" function to find the possiton between two cells
distanceBtwnCells :: Cell -- ^ uses the positon in the cell's data 
                  -> Cell 
                  -> Double -- ^ distance between two cells as a double
distanceBtwnCells cell1 cell2 = pos cell1 `distance` pos cell2 

-- | Tests if two cells are intersecting by finding the distance between the
-- | cells and testing if that is less than the sum of their radii
intersect :: Cell -- ^ uses cell's Pos and mass data 
          -> Cell 
          -> Bool -- ^ intersection as a True or False value
intersect cell1 cell2 = cell1 `distanceBtwnCells` cell2 < radius cell1 + radius cell2 

-- | Uses the Pos of a mouse cursor to move the cell  
move :: Pos -- ^ Pos of the mouse cursor
     -> Cell -- ^ uses Pos of the cell
     -> Cell -- ^ creates a new cell with a Pos moving towards the cursor 
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
elemBy f x xs = (not.null) $ filter ((f x == ).f) xs 

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

main :: IO()
main = do
  let cell1 = Cell { name = "Hugh G. Rection" 
                   , cellNum = 0
                   , mass = 100
                   , pos = (0,10) 
                   , rgb = (0,0,0)
                   }
  let cell2 = Cell { name = "Not Hugh G. Rection"
                   , cellNum = 0
                   , mass = 10
                   , pos = (0,10) 
                   , rgb = (0,0,0)
                   }
  let cols = checkCollisions [cell1, cell2]
  let m = move (pos cell1) cell2
  print cols
  print ""
  print m


-- Heres the FunGEn URLs
-- https://github.com/simonmichael/fungen/blob/master/examples/pong/pong.hs
-- http://hackage.haskell.org/package/FunGEn-0.4.6.1/docs/Graphics-UI-Fungen.html