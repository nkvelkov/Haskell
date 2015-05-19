
data Tree a = Empty | Node a (Tree a) (Tree a)

level :: Int -> Tree a -> [a]
level _ Empty = []
level lev (Node a leftSubtree rightSubTree)
	| lev == 1 = [a]
	| otherwise = level (lev-1) leftSubtree ++ level (lev-1) rightSubTree
	
testTree :: Tree Int
testTree = Node 11
                (Node 7
                    (Node 4 Empty Empty)
                    (Node 9 Empty Empty))
                (Node 21 Empty Empty)
