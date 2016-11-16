data Count a = Single a | Multiple Int a deriving (Eq, Show)

decode' :: Count a -> [a]
decode' (Single x) = [x]
decode' (Multiple y x) = take y (repeat x)

decodeModified :: [Count a] -> [a]
decodeModified [] = error "Empty List"
decodeModified [x] = decode' x
decodeModified (x:xs) = (decode' x)++(decodeModified xs)