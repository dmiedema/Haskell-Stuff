import Control.Monad
import Data.List as List
import Data.Sequence as Seq

data Node = Node { activity :: String,
					value :: Int,
					used :: Bool
					} deriving (Show, Eq, Ord)

main = do
	ln <- getLine
	let count = read ln :: Int
	activities <- forM [count, count-1..1] (\a -> do
		activity <- getLine
		return activity)
	let nodes = fromList (map createNode (map createTuple (List.sort activities)))
	--print activities
	let paritionedValues = List.partition (>0) (map getValues activities)
	print paritionedValues
	--print nodes
	--print (Seq.length nodes)

createTuple :: String -> (String, String)
createTuple string = List.splitAt (head (elemIndices ' ' string)) string

getValues :: String -> Int
getValues string = read (snd (List.splitAt (head (elemIndices ' ' string)) string)) :: Int

createNode :: (String,String) -> Node
createNode split =  Node {activity = (fst split), value = (read (snd (split)) :: Int), used = False}


-- A* - each value is a node, goal is 0	