import Control.Monad
import Data.List

main = do
	ln <- getLine
	let count = read ln :: Int
	activities <- forM [count, count-1..1] (\a -> do
		activity <- getLine
		return activity)
	let listOfActivites = map createTuple (sort activities)
	let values = partition (>0) (map extractValues activities)

	print listOfActivites
	print values




createTuple :: String -> (String, String)
createTuple string = splitAt (head (elemIndices ' ' string)) string

extractValues :: String -> Int
extractValues string = read (snd (splitAt (head (elemIndices ' ' string)) string)) :: Int

--getValues :: [(String, String)] -> [Int]
--getValues listOfTuples = 
--	x = head listOfTuples
--	xs = tail listOfTuples



--findSolution :: [Int] -> [Int]
--findSolution activities = 

-- A* - each value is a node, goal is 0
--partition (>0) <list>

	--read $ snd $ head [("something", "-14")] :: Int