module Main where

fatorial n
	| n==0 = 1
	| n==1 = 1
	| otherwise = n * fatorial (n-1)

main = do
	putStrLn "Digite um número: "
	numero <- getLine
	let n = read numero::Integer
	let resultado = fatorial n
	putStrLn ("Fatorial de " ++ (show n) ++ " é " ++ (show resultado))