data Tree a = Empty | Node a (Tree a) (Tree a) -- deriving Show

member:: (Eq a) => a -> Tree a -> Bool
member _ Empty = False
member elem (Node k leftSubTree rightSubTree)
	| elem == k = True	
	| otherwise = (member elem leftSubTree) || (member elem rightSubTree)

member':: (Ord a) => a -> Tree a -> Bool
member' _ Empty = False
member' elem (Node k leftSubTree rightSubTree)
	| elem == k = True
	| elem < k = member' elem leftSubTree
	| otherwise = member' elem rightSubTree
inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node elem leftSubTree rightSubTree) = (inOrder leftSubTree) ++ [elem] ++ (inOrder rightSubTree)

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node elem leftSubTree rightSubTree) = (postOrder leftSubTree) ++ (postOrder rightSubTree) ++ [elem]

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node elem leftSubTree rightSubTree) = [elem] ++ preOrder leftSubTree ++ preOrder rightSubTree

insert :: (Ord a) => a -> Tree a -> Tree a
insert elem Empty = Node elem Empty Empty
insert elem (Node k leftSubTree rightSubTree)
	| elem < k = Node k (insert elem leftSubTree) rightSubTree
	| otherwise = Node k leftSubTree (insert elem rightSubTree) 

sumTree :: Tree Int -> Int
sumTree Empty = 0
sumTree (Node elem leftSubTree rightSubTree) = elem + sumTree leftSubTree + sumTree rightSubTree

sumTree' :: Tree Int -> Int
sumTree' tree = sum (inOrder tree)

minTree':: (Ord a) => Tree a -> a
minTree' tree = minimum (inOrder tree)

data Order = Online Float Int Int | Offline Float

type ShoppingCard = [Order]

shopping = [(Offline 2) (Online 1.2 1 1)]

onlineOrders:: ShoppingCard -> Int
onlineOrders [] = 0
onlineOrders (x:xs) = 1 + onlineOrders xs -- ((Online price id hours): xs)

data Expr = I Int | Add Expr Expr | Mult Expr Expr deriving Show

eval:: Expr -> Int
eval (I num) = num
eval (Add e1 e2) = eval e1 + eval e2
eval (Mult e1 e2) = eval e1 * eval e2
-- minTree:: (Ord a) => Tree a
-- minTree (Node elem Empty Empty) = elem 
-- minTree (Node k leftSubTree rightSubTree)
	-- | elem < k = 
	-- | 
	-- where

testTree:: Tree Int 	
testTree = Node 7 
			(Node 5 (Node 3 Empty Empty) 
				 (Node 6 Empty Empty))
			(Node 11 Empty Empty)

testTree1:: Tree Int
testTree1 = Node 5 
				(Node 3 Empty Empty)
				(Node 7 Empty Empty)
findWhere :: (a->Bool) -> [a] -> a
findWhere _ [] = error "exception"
findWhere pred (x:xs)
    | pred x = x
    | otherwise = findWhere pred xs