range :: Int -> Int -> [Int]
range x y =	if x == y
	  	  	then [x]
			else if x < y
	  	  		 then x:(range (x+1) y)
				 else x:(range (x-1) y)