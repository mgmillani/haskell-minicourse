{-module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

-- import Figures


main = do
	(progname, _) <- getArgsAndInitialize
  createWindow "Hello World"
  displayCallback $= display
  mainLoop

display = do
	clear [ColorBuffer]
  flush
		-}

module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

precision = 32

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = map (\k -> (sin(2*pi*k/precision),cos(2*pi*k/precision),0.0)) [1..precision]
main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Hello World"
  displayCallback $= display myPoints
  mainLoop
display points = do
  clear [ColorBuffer]
  renderPrimitive LineLoop $ do
  	color $(Color3 (1.0::GLfloat) 0 0)
  	mapM_ (\(x, y, z)->vertex$Vertex3 x y z) points
  flush