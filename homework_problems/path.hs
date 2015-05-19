
data Tree a = Empty | Node a (Tree a) (Tree a)

pathTo :: (Ord a) => a -> Tree a -> [a]
pathTo _ Empty = error "exception, no path"
pathTo elem (Node a leftSubTree rightSubTree) 
	| a == elem = [a]
	| a > elem = a : pathTo elem leftSubTree 
	| otherwise = a : pathTo elem rightSubTree

	
find :: (a->Bool) -> [a] -> [a]
find _ [] = error "No such element"
find pred (x:xs)
    | pred x = x : find pred xs
    | otherwise = find pred xs

testTree :: Tree Int
testTree = Node 11
                (Node 7
                    (Node 4 Empty Empty)
                    (Node 9 Empty Empty))
                (Node 21 Empty Empty)