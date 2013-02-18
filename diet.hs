import Control.Monad
import Data.List

data Node = Node { activity :: String,
					value :: Int,
					used :: Bool
					} deriving (Show)

main = do
	ln <- getLine
	let count = read ln :: Int
	activities <- forM [count, count-1..1] (\a -> do
		activity <- getLine
		return activity)
	let listOfActivites = map createTuple (sort activities)
	let nodes = map createNode listOfActivites
	print nodes

	--print listOfActivites
	--print values

	--let solution = findSolution values
	--print solution

createTuple :: String -> (String, String)
createTuple string = splitAt (head (elemIndices ' ' string)) string

createNode :: (String,String) -> Node
createNode split =  Node {activity = (fst split), value = (read (snd (split)) :: Int), used = False}
	--split = splitAt (head (elemIndices ' ' string)) string
	


--findSolution :: [Int] -> [Int]
--findSolution list = let tuple = partition (>0) list
--	| fst tuple == []
--	| snd tuple == []

		--negatives = snd tuple
	--in negatives

--filter (== 0) (snd $ mapAccumL (\ x y -> (x,x+y)) 2 [1,2,3,-3,-4,-1])

-- A* - each value is a node, goal is 0	