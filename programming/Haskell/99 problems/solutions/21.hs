insertAt :: a -> [a] -> Int -> [a]
insertAt x xs y = (take (y-1) xs)++[x]++(drop (y-1) xs)