myLength :: (Num b, Eq b) => [a] -> b
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)