module Main	where
	
main = do
	file <- readFile "arquivos.hs"
	putStrLn file
	
