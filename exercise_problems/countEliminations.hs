
countElimination :: [Int] -> Int
countElimination [] = 0
countElimination list = countEliminationsH list 0

countEliminationsH :: [Int] -> Int -> Int
countEliminationsH list k 	
	| list == [] = k
	| not $canEliminate list = k
	| otherwise = countEliminationsH (eliminate list) (k+1)

canEliminate :: [Int] -> Bool
canEliminate l = canEliminateH l (maximum l)

canEliminateH :: [Int] -> Int -> Bool
canEliminateH [] _ = False
canEliminateH [x] _ = False
canEliminateH (x:y:xs) m 
	| (x == m) && (x == y) = True
	| otherwise = canEliminateH (y:xs) m

eliminate :: [Int] -> [Int]
eliminate l = eliminateH l (maximum l)	

eliminateH:: [Int] -> Int -> [Int]
eliminateH [] _ = []
eliminateH [x] _ = [x]
eliminateH (x:y:xs) m 
	| (x == y) && (x == m) = xs
	| otherwise = x : eliminateH (y:xs) m