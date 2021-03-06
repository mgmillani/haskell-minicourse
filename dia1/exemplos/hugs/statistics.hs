module Statistics where

average l = (foldl (+) 0 l) / (foldl (\x y -> x+1) 0 l)

{-
variance l =
	let avg = average l
	    dif = map (\x -> (x - avg)^2) l
	in average dif
	-}

variance l =
	average dif
	where avg = average l
	      dif = map (\x -> (x - avg)^2) l
	
deviation l = sqrt $ variance l