module Drawable (Drawable(..)) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

convertToGLfloat x = (fromRational . toRational $ x)::GLfloat

convertPoint (x,y) = Graphics.UI.GLUT.vertex $ Vertex2 (convertToGLfloat x) (convertToGLfloat y)

class Drawable a where
	vertex :: (Drawable a, Floating b) => a -> (b,b) -> [(b,b)]
	{-draw :: (Drawable a,Floating b) => PrimitiveMode -> a -> (b,b) -> IO ()
	drawWire :: (Drawable a, Floating b) => a -> (b,b) -> IO ()
	draw mode figure point = renderPrimitive mode $ mapM_ convertPoint (Drawable.vertex figure point)
	drawWire figure point = draw LineLoop figure point-}
	--fill a = draw Polygon a