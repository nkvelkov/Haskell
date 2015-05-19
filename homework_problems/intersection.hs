intersection :: (Eq a) => [a] -> [a] -> [a]
intersection [] [] = []
intersection [] _ = []
intersection _ [] = []
intersection (x:xs) (y:ys) 
	| elem x (y:ys) = x : intersection (xs) (filterFirst (\e -> e == x) (y:ys) )
	| otherwise = intersection (xs) (y:ys)

filterFirst :: (a->Bool) -> [a] -> [a]
filterFirst _ [] = []
filterFirst f (x:xs)
	| f x = xs
	| otherwise = x : filterFirst f xs
	
intersection'' :: (Eq a) => [a] -> [a] -> [a]
intersection'' list1 list2 = [x | x <- list1, y <- list2, x == y]

