-- These are the the player cells 
module Types.Cells where

type Position = (Double, Double)

type Cells = [Cell]

data Cell = Cell { name :: String
                 , alive :: Bool
                 , deaths :: Int
                 , mass :: Int
                 , pos :: Position 
                 } deriving (Show, Eq)

radius :: Cell -> Double
radius (Cell { mass = m }) = sqrt(fromIntegral m / pi)

distance :: Position -> Position -> Double
distance (x,y) (x2,y2) = sqrt((x2-x)**2 + (y2-y)**2)

distanceBtwnCells :: Cell -> Cell -> Double
distanceBtwnCells c1 c2 = pos c1 `distance` pos c2 

intersect :: Cell -> Cell -> Bool
intersect cell1 cell2 = cell1 `distanceBtwnCells` cell2 < radius cell1 + radius cell2 

threshold = 10
upperSpeed = 10
lowerSpeedScale = 0.1

-- I would recommend a type signature like this so that you can 
-- differentiate which cell was absorbed into which if any were absorbed at all
-- Remember that Maybes can have a value of either (Just x) where x is a Cell or 
-- Nothing without a value
-- Of course if you want something else that is totally possible
-- Theres more than one way to do anything 
-- collide :: Cell -> Cell -> (Maybe Cell, Maybe Cell)

-- getX, getY, collide, checkCollisions :: Cells -> Cells

-- listComprehension = [ x * y | x <- xs, y <- ys, x /= y]
-- Means multiply all values in xs by all other values in ys as long as the two 
-- values are not equal

-- Let me know if theres something I missed.  Good luck 


-- | Comments look like this, if you want more information lookup `haddock` 
-- comments
move :: Position -> Cell -> Cell
move cursor@(curX, curY) cell = cell { pos = (x + dx, y + dy)} 
  where l = cursor `distance` (pos cell)
        progress = if l > threshold then upperSpeed else l*lowerSpeedScale
        (x,y) = pos cell
        theta = atan ((y - curY) / (x - curX))
        dx = cos theta * progress
        dy = sin theta * progress

main :: IO()
main = do
  let cell1 = Cell { name = "Hugh G. Rection"
                   , alive = True
                   , deaths = 0
                   , mass = 100
                   , pos = (7.426019951630333,10) 
                   }
  let cell2 = Cell { name = "Not Hugh G. Rection"
                   , alive = True
                   , deaths = 0
                   , mass = 10
                   , pos = (0,10) 
                 }
  let inter = cell1 `intersect` cell2
  print (inter, cell1 `distanceBtwnCells` cell2, (radius cell1 + radius cell2))