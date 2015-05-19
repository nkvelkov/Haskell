
reversibleNumbers :: Int -> [Int]
reversibleNumbers n = [ x | x <- [1..n], isRevisible x]

isRevisible:: Int -> Bool
isRevisible 0 = False
isRevisible n = (not (hasLeadingZeroes n)) && hasOnlyOddDigits (n + (listToNum (reverse (numToList n)) ))

reverseNum :: Int -> Int
reverseNum 0 = 0
reverseNum n = (rem n 10) * 10 + reverseNum (div n 10)

numToList :: Int -> [Int]
numToList 0 = []
numToList n = (rem n 10) : numToList (div n 10)

listToNum :: [Int] -> Int
listToNum [] = 0
listToNum (x:xs) = x + 10 * listToNum xs

hasOnlyOddDigits :: Int -> Bool
hasOnlyOddDigits 0 = True
hasOnlyOddDigits n = (odd (rem n 10) ) && hasOnlyOddDigits (div n 10)

hasLeadingZeroes :: Int -> Bool
hasLeadingZeroes 0 = True
hasLeadingZeroes n
	| (rem n 10) == 0 = True
	| otherwise = False