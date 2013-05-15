module Drawable where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

type DrawVertex = (GLfloat,GLfloat)

class Drawable a where
	vertex :: Drawable a => a -> (GLfloat,GLfloat) -> [DrawVertex]
	draw :: Drawable a => PrimitiveMode -> a -> (GLfloat,GLfloat) -> IO ()
	drawWire :: Drawable a => a -> (GLfloat,GLfloat) -> IO ()
	draw mode figure point = renderPrimitive mode $ mapM_ (\(x, y)->Graphics.UI.GLUT.vertex$Vertex3 x y 0) (Drawable.vertex figure point)
	drawWire figure point = draw LineLoop figure point
	--fill a = draw Polygon a