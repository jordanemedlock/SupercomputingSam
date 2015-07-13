-- These are the little cells that do not move, but are
-- used by the player cell in order to gain mass
module Types.Fare where

import Types.Cells hiding (main)
import Control.Applicative
import System.Random
import Data.List
import qualified Data.List.Split as S

data Fare = Fare { cells :: Cells } deriving (Show, Eq)

generateRandomPopulation :: Double -> Double -> IO Cells
generateRandomPopulation xrange yrange = do
  let names = repeat "fare"
  let cellNums = repeat (-1)
  gen1 <- newStdGen
  let masses = randomRs (0,100) gen1
  gen2 <- newStdGen
  let xs = randomRs (0.0,xrange) gen2
  gen3 <- newStdGen
  let ys = randomRs (0.0,yrange) gen3
  let positions = zip xs ys
  return $ Cell <$> names <*> cellNums <*> masses <*> positions


main :: IO()
main = do
  randCells <- take 100 <$> generateRandomPopulation 190 100
  let food = Fare { cells=randCells }
  print food
