square :: Double -> Double
square x = x * x

pythag :: Double -> Double -> Double
pythag a b = sqrt (square a + square b)

square' :: (Num a) => a -> a
square' x = x * x

pythag2 :: (Floating a) => a -> a -> a
pythag2 a b = 
	--a :: Floating
	--b :: Floating
	sqrt ( square' a + square' b)

isEven :: Integer -> Bool
isEven x 
	| x `mod` 2 == 0 = True
	| otherwise 	 = False

factorial :: Integer -> Integer
factorial x = foldr (*) 1 [x,x-1..1]
		-- foldr (binary function to perform), starting value, list to perform it on


filter (/= 0) (map (x `mod`) [sqrt x, (sqrt x) -1 .. 2])

 [x | x <- [1..100], filter (/=0) (map (2  `mod`) [(sqrt  (fromIntegral x)), (sqrt (fromIntegral x) - 1)..2]) ]

isPrime :: Integer -> Bool
isPrime x = 
	let x = abs x
	| x < 2  		 = False
	| x == 2 		 = True
	| x `mod` 2 == 0 = False
	| otherwise 

--isInt :: Floating -> Bool
isInt x = x == fromInteger (round x)
 