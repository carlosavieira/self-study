take' :: [a] -> Int -> [a]
take' [] _ = []
take' _ 0 = []
take' (x:xs) y = x:(take' xs (y-1))

drop' :: [a] -> Int -> [a]
drop' [] _ = []
drop' xs 0 = xs
drop' (x:xs) y = drop' xs (y-1)

split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs y = (take' xs y, drop' xs y)