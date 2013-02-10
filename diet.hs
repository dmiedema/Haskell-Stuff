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

findSolution :: ([Int], [Int]) -> [Int]
findSolution tuple = 
	let positives = fst tuple
	let negatives = snd tuple


--filter (== 0) (snd $ mapAccumL (\ x y -> (x,x+y)) 2 [1,2,3,-3,-4,-1])

-- A* - each value is a node, goal is 0	