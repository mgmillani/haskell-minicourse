module Main where

	fatorial 0 = 1
	fatorial 1 = 1
	fatorial n = n * fatorial (n-1)

	main = do
		numero <- getLine
		let n = read numero::Integer
		let resultado = fatorial n
		putStrLn ("Fatorial de " ++ (show n) ++ " é " ++ (show resultado))