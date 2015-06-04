module Types.Cells where

type Position = (Double, Double)

type Cells = [Cell]

data Cell = Cell { name :: String
                 , alive :: Bool
                 , deaths :: Int
                 , mass :: Int
                 , pos :: Position 
                 } deriving (Show, Eq)

-- | Finds the radius of a circular Cell using its mass as the Area by finding
-- | the square root of the area over pi 
radius :: Cell -- ^ contains the data of a cell to find the
       -> Double -- ^ radius as a double
radius (Cell { mass = m }) = sqrt(fromIntegral m / pi)

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
distanceBtwnCells c1 c2 = pos c1 `distance` pos c2 

-- | Tests if two cells are intersecting by finding the distance between the
-- | cells and testing if that is less than the sum of their radii
intersect :: Cell -- ^ uses cell's position and mass data 
          -> Cell 
          -> Bool -- ^ intersection as a True or False value 
intersect cell1 cell2 = cell1 `distanceBtwnCells` cell2 < radius cell1 + radius cell2 

-- Finds the x or the y coordinate of every cell in a list
getX, getY :: Cells 
           -> [Double] -- ^ x or y coordinate as a list of doubles
getX cells = [fst (pos cell) | cell <- cells]
getY cells = [snd (pos cell) | cell <- cells]

-- I didn't know which of these I would need/want more... so I made them both

-- | Finds the x and the y cordinate of every cell in a list
getXY :: Cells 
      -> [Position] -- ^ x and y as a list of the "Positiion" type
getXY cells = [pos cell | cell <- cells] 

-- | Finds and returns a cell with the mass between two cells
greaterMass :: Cell -- ^ uses the mass of two cells
            -> Cell 
            -> Cell -- ^ larger cell as a cell
greaterMass cell1 cell2 = if mass cell1 > mass cell2 then cell1 else cell2

-- | Sees if two cells are intersecting or not, and if they are, then makes the 
-- | smaller cell Nothing and gives the larger cell the smaller's mass
absorb :: Cell -- ^ uses cells mass, and the "intersect" function 
       -> Cell 
       -> (Maybe Cell, Maybe Cell) -- ^ absorbed cell as a tuple with Just cell or Nothing
absorb cell1 cell2 
    | inters && mass cell1 > mass cell2 = (Just cell1 {mass = newMass}, Nothing)
        -- returns Just cell1 with the newMass when cell1 is larger and they intersect 
    | inters && mass cell1 < mass cell2 = (Nothing, Just cell2 {mass = newMass})
        -- Does the same as above except using cell2 instead 
    | otherwise                         = (Just cell1, Just cell2)
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
  print (inter, cell1 `distanceBtwnCells` cell2, radius cell1 + radius cell2)