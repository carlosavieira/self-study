dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs y = (take (y-1) xs)++(dropEvery (drop y xs) y)