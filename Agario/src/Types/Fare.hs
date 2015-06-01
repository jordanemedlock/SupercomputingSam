-- These are the little cells that do not move, but are 
-- used by the player cell in order to gain mass
module Types.Fare where

	import Types.Cells -- Whelp... this doesn't work :'( This is frustrating 
					   -- And when Jordan explains why it's not working, he's gonna be
					   -- like, "oh, your flux capacitor intersected with the quantum infrastructure
					   -- of the Afro-Eurasia bose-einstein condensate which makes your list fail to 
					   -- comply with the benevolent camaraderie between the program, file, and ghc."

	data Fare = Fare { cells :: [Cell] } deriving (Show, Eq)

	main :: IO()
	main = do
		let food = Fare { cells=[] }
		print food