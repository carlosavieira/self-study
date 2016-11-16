removeAt :: Int -> [a] -> (a, [a])
removeAt x xs = (xs !! (x-1), (take (x-1) xs)++(drop x xs))