module Types.Graveyard where

import Types.People hiding (main)

data Graveyard = Graveyard { people :: [Person] } deriving (Show, Eq)

addGrave :: Person -> Graveyard -> Graveyard
addGrave p g = Graveyard (kill p:people g)
  
main = do
  let p1 = Person {name="Guy the first",role="gonna die", alive=True}
  let p2 = Person {name="Guy the other",role="killer",alive=True}
  let gy = Graveyard {people = []}
  print p1
  print p2
  print gy
  print $ alive $ head $ people $ addGrave p1 gy
