module Main where

data Figura a = Circulo a | Retangulo a a

precisao = 6

pontoCirculo r k = (r * (cos $ 2 * pi * k / precisao) , r * (sin $ 2 * pi * k / precisao))

vertices (Retangulo largura altura) (x,y) =
	[(x+l2,y+a2),(x-l2,y+a2),(x-l2,y-a2),(x+l2,y-a2)]
	where
		l2 = largura/2
		a2 = altura/2

vertices (Circulo r) (x,y) =
	map (\k -> let (a,b)= pontoCirculo r k in (x+a,y+b) ) [1..precisao]

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
