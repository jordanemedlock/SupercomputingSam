-- These are the the player cells 
module Types.Cells where

	type Cells = [Cell]

	data Cell = Cell { name :: String, alive :: Bool, deaths :: Int, mass :: Int } deriving (Show, Eq)

	main :: IO()
	main = do
		let cell = Cell { name = "Hugh G. Rection", alive = True, deaths = 0, mass = 100 }
		print cell