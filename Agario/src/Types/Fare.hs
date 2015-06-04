-- These are the little cells that do not move, but are 
-- used by the player cell in order to gain mass
module Types.Fare where

  import Types.Cells 

  data Fare = Fare { cells :: [Cell] } deriving (Show, Eq)

  main :: IO()
    main = do
    let food = Fare { cells=[] }
    print food