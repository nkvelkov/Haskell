lca :: (Ord a) => a -> a -> Tree a -> a
lca elem1 elem2 tree = getLastCommon list1 list2
	where 
		list1 = pathTo elem1 tree
		list2 = pathTo elem2 tree
		
getLastCommon :: (Eq a) => [a] -> [a] -> a
getLastCommon (x:xs) (y:ys) = getLastCommonHelper xs ys x

getLastCommonHelper :: (Eq a) => [a] -> [a] -> a -> a
getLastCommonHelper [] _ prev = prev
getLastCommonHelper _ [] prev = prev
getLastCommonHelper (x:xs) (y:ys) prev 
	| x /= y = prev
	| otherwise = getLastCommonHelper xs ys x

data Tree a = Empty | Node a (Tree a) (Tree a)

pathTo :: (Ord a) => a -> Tree a -> [a]
pathTo _ Empty = error "exception, no path"
pathTo elem (Node a leftSubTree rightSubTree) 
	| a == elem = [a]
	| a > elem = a : pathTo elem leftSubTree 
	| otherwise = a : pathTo elem rightSubTree


testTree :: Tree Int
testTree = Node 11
                (Node 7
                    (Node 4 (Node 3 Empty Empty) (Node 5 Empty Empty))
                    (Node 9 Empty Empty))
                (Node 21 Empty Empty)