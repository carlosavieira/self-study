elementAt :: (Num b, Eq b) => [a] -> b -> a
elementAt [] _ = error "Element out of range"
elementAt (x:xs) 1 = x
elementAt (x:xs) y = elementAt xs (y-1)