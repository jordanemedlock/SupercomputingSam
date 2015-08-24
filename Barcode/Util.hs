import Data.List
data Color = Black | Red | Green | Blue  deriving (Show, Eq, Ord, Enum)
code :: [((Int,Int),Color)] -> Int
code = toBaseTen.toBaseFourString


toBaseFourString :: [((Int,Int),Color)] -> String
toBaseFourString [] = []
toBaseFourString dots = char : rest
  where dot@(_,c) = head $ furthest up $ furthest left dots
        char = head $ show $ fromEnum c
        rest = toBaseFourString $ delete dot dots

toBaseTen :: String -> Int
toBaseTen = sum.zipWith (*) fours.reverse.map (read.(:[])) 
  where fours = map (4^) [0..]

furthest dir xs = foldr dir [] xs

left p [] = [p]
left p@((x,_),_) xs = case compare x (fst $ fst $ head xs) of
            LT -> [p]
            EQ -> p : xs
            GT -> xs

up p [] = [p]
up p@((_,y),_) ys = case compare y (snd $ fst $ head ys) of
            LT -> [p]
            EQ -> p : ys
            GT -> ys
