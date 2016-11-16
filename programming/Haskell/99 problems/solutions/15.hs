repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) y = (take y (repeat x))++(repli xs y)