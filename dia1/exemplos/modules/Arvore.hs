module Arvore where

data Tree a = Leaf | Node (Tree a) a (Tree a)

abpInsere (Node left key right) value =
	if value < key
		then Node (abpInsere left value) key right
	else Node left key (abpInsere right value)
abpInsere Leaf value = Node Leaf value Leaf

abpImprime Leaf = putStr ""
abpImprime (Node left key right) = do
	abpImprime left
	print key
	abpImprime right
	