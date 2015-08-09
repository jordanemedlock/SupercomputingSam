-- These are the little cells that do not move, but are
-- used by the player cell in order to gain mass
module Types.Fare where

import Types.Cells hiding (main)
import Control.Applicative
import System.Random
import Data.List
import qualified Data.List.Split as S

data Fare = Fare { cells :: Cells } deriving (Show, Eq)

generateRandomPopulation :: Double -> Double -> Int -> IO Cells
generateRandomPopulation xrange yrange massRange = do
  gen <- newStdGen
  let names = S.splitOn "a" $ randomRs ('a','z') gen
  let cellNums = map negate [1..]
  gen1 <- newStdGen
  let masses = randomRs (0,massRange) gen1
  gen2 <- newStdGen
  let xs = randomRs (0.0,xrange) gen2
  gen3 <- newStdGen
  let ys = randomRs (0.0,yrange) gen3
  let positions = zip xs ys
  gen4 <- newStdGen
  let rs = randomRs (0.0,1.0) gen4
  gen5 <- newStdGen
  let gs = randomRs (0.0,1.0) gen5
  gen6 <- newStdGen
  let bs = randomRs (0.0,1.0) gen6
  let rgbs = zip3 rs gs bs
  return $ zipWith5 Cell names cellNums masses positions rgbs

main :: IO()
main = do
  randCells <- take 100 <$> generateRandomPopulation 100 100 100
  let food = Fare { cells=randCells }
  print randCells
