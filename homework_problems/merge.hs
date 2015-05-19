
merge :: [a] -> [a] -> [a]
merge [] [] = []
merge (x:xs) [] = (x:xs)
merge [] (x:xs) = (x:xs) 

merge (x:xs) (y:ys) = concat $mergeHelper (x:xs) (y:ys)

mergeHelper :: [a] -> [a] -> [[a]]
mergeHelper xs ys = [ [x, y] | (x, y) <- zip xs ys ]
 

