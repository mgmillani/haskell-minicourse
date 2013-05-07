module Main where

import Arvore

main = do
	let a = abpInsere Leaf 3
	let b = abpInsere a 2
	let c = abpInsere b 1
	let d = abpInsere c 4

	print "C:"
	abpImprime c

	print "D:"
	abpImprime d