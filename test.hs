import Control.Monad

factorial :: Integer -> Integer
factorial x = foldr (*) 1 [x,x-1..1]

main = factorial 100000
--main = do 
--	l <- getLine
--	factorial 100000