module Main where

import Figura

ca = Circulo 1
cb = Circulo 2
ra = Retangulo 1 1
rb = Retangulo 2 1

distance (a,b) (c,d) =
	sqrt $ ((a-c)^2 + (b-d)^2)

main = do
	putStrLn "Ca"
	let va = vertices ca (0,0)
	mapM_ print $ map (distance (0,0)) va

	putStrLn "\nCb"
	let vb = vertices cb (0,0)
	mapM_ print $ map (distance (0,0)) vb

	putStrLn "\nRa"
	let vc = vertices ra (0,0)
	mapM_ print vc

	putStrLn "\nRb"
	let vd = vertices rb (0,0)
	mapM_ print vd
