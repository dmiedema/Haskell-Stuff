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
	let paritionedValues = List.partition (>0) (map getValues activities)
	let listOfValues = map getValues activities
	print paritionedValues
	print (checkOppositeValues paritionedValues)
		{- running subsequence (permutations) will be a pain to work with.
		 Might be easier to permute and run subsequence indiviually.
		 I don't know.
		-}
	let holyCrapThisIsHuge =  (permutations listOfValues)
	--print nodes
	--print (Seq.length nodes)

createTuple :: String -> (String, String)
createTuple string = List.splitAt (head (elemIndices ' ' string)) string

getValues :: String -> Int
getValues split = read (snd (createTuple split)) :: Int

createNode :: (String,String) -> Node
createNode split =  Node {activity = (fst split), value = (read (snd (split)) :: Int), used = False}

checkOppositeValues :: ([Int], [Int]) -> Bool
checkOppositeValues tuple = 
	Seq.length (Seq.fromList(fst tuple ++ (map (abs) (snd tuple)))) == 
		Seq.length (Seq.fromList( List.nub (fst tuple ++ (map (abs) (snd tuple)))))
 
 --- this is possibly the scaries thing ive ever made. Wow this is going to be terrible.
-- subsequence (permutations list) is a horrible, horrible idea. but it will bruteforce it.

--tryEverything :: [Int] -> [Int]
--tryEverything bigList =

 -- Sequence of Nodes, current value, number of nodes used -> Returns Sequence of Nodes
iterateThroughList :: (Seq Node seqN) => seqN -> Int -> Int -> seqN
iterateThroughList seqN currentValue numberOfNodesUsed
	| empty seqN = fromList ["No Solution"]
	| currentValue == 0 -- find all nodes in seqN that have used==True
	| (numberOfNodesUsed == length seqN) && (currentValue /= 0) = fromList ["No Solution"] -- try again by dropping a node
	| otherwise -- do this again, find a node that is used==False and apply its value to currentValue

-- made changes holy crap did i make changes. WOOT CHANGES
	-- Merge lists and covert all number to positive numbers
	-- fst tuplelist ++ (map (abs) (snd tuplelist))

-- A* - each value is a node, goal is 0	

-- Holy derp.
-- map (== (read (\x -> map (head) (fst tuplelist))::Int)) (fst tuplelist  ++ (map (abs) (snd tuplelist )))
		{-
		The lambda expression `\ x -> map (head) (fst tuplelist)'
		    has one argument,
		    but its type `String' has none
		    In the first argument of `read', namely
		      `(\ x -> map (head) (fst tuplelist))'
		    In the second argument of `(==)', namely
		      `(read (\ x -> map (head) (fst tuplelist)) :: Int)'
		    In the first argument of `map', namely
		      `(== (read (\ x -> map (head) (fst tuplelist)) :: Int))'
		-}
