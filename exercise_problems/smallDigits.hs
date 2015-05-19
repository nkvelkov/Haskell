
smallDigits :: Int -> (Int -> Int)
smallDigits n = (\ e -> (sum ( [ head (smallerl (factors x) e) | x <- [1..n], ((smallerl (factors x) e) /= []) ]  ) ) )


factors :: Int -> [Int]
factors 1 = [0]
factors n = [ x | x <- [2..n], rem n x == 0]

factorss :: Int -> Int -> [Int]
factorss n e = tail [ (head $factors x) | x <- [1..n], smaller (head $factors x) e]

smallerl :: [Int] -> Int -> [Int]
smallerl [] _ = []
smallerl (x:xs) k 
	| smaller x k = x : smallerl xs k
	| otherwise = smallerl xs k 

smaller :: Int -> Int -> Bool
smaller 0 _ = True
-- smaller 1 _ = True
smaller n k
	| (rem n 10) > k = False	
	| otherwise = smaller (div n 10) k