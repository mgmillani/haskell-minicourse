module Main where

a = [1..10]
b = "Hello"

main = do
	print $ foldr (-) 100 a
	print $ foldl (-) 100 a
	
	mapM_ (putStr . show) a
	putStr "\n"

	mapM_ (putStr . (++ " ") . show) (map (*2) a)
	putStr "\n"