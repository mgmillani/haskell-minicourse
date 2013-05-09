module Main where

import Graphics.HGL

fillTri w x y size =
	polygon [(x,y),(x+size,y),(x,y-size)]

sierpinskiTri w x y size minSize = 
	if size < minSize
		then fillTri w x y size
	else let size2 = size `div` 2 in do
					sierpinskiTri w x y size2 minSize
					sierpinskiTri w x (y-size2) size2 minSize
					sierpinskiTri w (x+size2) y size2 minSize
	
poly = [(20,20)::Point,(40,20),(30,60),(90,100)]

pic1 = withColor Red (polygon poly)

main = do	
	runGraphics $
		withWindow_ "Hello World Window" (300, 200) $ \ w -> do
			-- drawInWindow w $ text (100, 100) s
			-- drawInWindow w $ ellipse (100, 80) (200, 180)
			-- drawInWindow w pic1
			drawInWindow w $ withColor Blue $ sierpinskiTri w 10 200 128 16
			getKey w
			closeWindow w