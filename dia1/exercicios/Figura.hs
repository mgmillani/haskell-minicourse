module Figura where

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
