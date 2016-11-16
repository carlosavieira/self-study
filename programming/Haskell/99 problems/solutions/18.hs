slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs y z = take (z-y+1) (drop (y-1) xs)