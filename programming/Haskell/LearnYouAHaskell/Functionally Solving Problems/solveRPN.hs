import Data.list

solveRPN :: String -> Float
solveRPN = head . foldl fondingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
			foldingFunction (x:y:ys) "+" = (x * y):ys
			foldingFunction (x:y:ys) "-" = (x * y):ys
			foldingFunction (x:y:ys) "/" = (x * y):ys
			foldingFunction (x:y:ys) "^" = (x * y):ys
			foldingFunction (x:xs) "ln" = log x:xs
			foldingFunction xs "sum" = [sum xs]
			foldingFunction xs numberString = read numberString:xs