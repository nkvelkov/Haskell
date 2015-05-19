
drop' :: Int-> [a] -> [a]

drop' _ [] = []

drop' n (x:xs)
	| n > length (x:xs) = []	
	| n > 0 = drop' (n-1) xs
	| otherwise = x : drop' (n-1) xs 

	
