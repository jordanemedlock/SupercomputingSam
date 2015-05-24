module Types.People where

type People = [Person]

data Person = Person { alive :: Bool, name :: String, role :: String } deriving (Eq)

instance Show Person where
  show p = name p ++ "(" ++ role p ++ ")"

kill :: Person -> Person
kill p1 = p1 { alive = False }



main :: IO ()
main = do
  let guy = Person { alive = True, role = "Revolutionary", name = "Guy" } 
  print (name guy)
  print guy