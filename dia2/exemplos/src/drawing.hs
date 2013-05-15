module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Figures

ca = Circle 0.2

main = do
	(progname, _) <- getArgsAndInitialize
  createWindow "Hello World"
  displayCallback $= display ca
  mainLoop

display = do
	clear [ColorBuffer]
	draw ca (0,0)
  flush