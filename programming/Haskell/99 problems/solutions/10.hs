encode :: (Eq a) => [a] -> [(Integer, a)]
encode [] = []
encode [x] = [(1, x)]
encode (x:xs) = if x `elem` (head (encode xs))
                then (((fst (head (encode xs))) +1), snd (head (encode xs))):(tail (encode xs))
                else (1, x):(encode xs)