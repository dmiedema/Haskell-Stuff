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

