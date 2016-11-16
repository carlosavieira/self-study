rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs y = if y < 0
	   	  	  then (drop ((length xs)+y) xs)++(take ((length xs)+y) xs)
			  else (drop y xs)++(take y xs)