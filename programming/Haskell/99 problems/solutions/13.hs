data Count a = Multiple Integer a | Single a deriving (Show, Eq)

encodeDirect :: (Eq a) => [a] -> [Count a]
encodeDirect = encodeModified

add' :: Count a -> Count a
add' (Single x) = (Multiple 2 x)
add' (Multiple x y) = (Multiple (x+1) y)

elem' :: (Eq a) => a -> Count a -> Bool
elem' x (Multiple _ y) = (x==y)
elem' x (Single y) = (x==y)

encodeModified :: (Eq a) => [a] -> [Count a]
encodeModified [] = error "Empty List"
encodeModified [x] = [Single x]
encodeModified (x:xs) = if x `elem'` (head (encodeModified xs))
			   		  	then (add' (head(encodeModified xs))):(tail(encodeModified xs))
						else (Single x):(encodeModified xs)