module Main where

precisão = 64

data Figura a = Círculo a | Retângulo a a

pontoCírculo raio k = (raio * (sin $ 2*pi*k/precisão) , raio * (cos $ 2*pi*k/precisão))

vértices (Círculo raio) (x,y) =
	map (\k -> let (a,b) = pontoCírculo raio k in (a+x,b+y)) [1..precisão]

vértices (Retângulo l a) (x,y) =
	(x+l2,y+a2) : (x+l2,y-a2) : (x-l2,y-a2) : (x-l2,y+a2) : []
	where
		l2 = l/2
		a2 = a/2

printLista [] = putStrLn ""
printLista (h:rest) = do	
	print h
	printLista rest

distancia (a,b) (c,d) =
	let dx = a-c
	    dy = b-d
	in
		sqrt $ dx*dx + dy*dy


ca = Círculo 10
ra = Retângulo 5 10
main = do 
	let v = vértices ra (0,0)
	printLista v
	let d = map (distancia (0,0)) v
	printLista d