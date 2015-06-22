-- These are the little cells that do not move, but are 
-- used by the player cell in order to gain mass
module Types.Fare where

import Types.Cells hiding (main)
import Control.Applicative
import System.Random
import Data.List
import qualified Data.List.Split as S

data Fare = Fare { cells :: Cells } deriving (Show, Eq)

generateRandomPopulation :: IO Cells
generateRandomPopulation = do
  let names = repeat "fare"
  let cellNums = repeat (-1)
  gen1 <- newStdGen
  let masses = randomRs (0,100) gen2 -- make porportional 
  gen2 <- newStdGen
  let xs = randomRs (0.0,100.0) gen3 -- make porportional 
  gen3 <- newStdGen
  let ys = randomRs (0.0,100.0) gen4 -- make porportional
  let positions = zip xs ys
  return $ zipWith5 Cell names alives cellNums masses positions 


main :: IO()
main = do
  randCells <- take 10 <$> generateRandomPopulation
  let food = Fare { cells=randCells }  
  print food