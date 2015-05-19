import Data.Char

lengthEncode :: String -> String
lengthEncode str = lengthEncodeHelper str 0
	
lengthEncodeHelper :: String -> Int -> String
lengthEncodeHelper [] _ = []
lengthEncodeHelper [x] _ = [x]
lengthEncodeHelper (x:y:xs) ctr
	| x == y && length (x:y:xs) == 2 = getMinString (ctr+2) x 
	| x == y = lengthEncodeHelper(y:xs) (ctr+1)
	| otherwise = (getMinString (ctr+1) x) ++ lengthEncodeHelper(y:xs) 0

getMinString :: Int -> Char -> String
getMinString n ch 
	|(length ((intToChar n) ++ [ch])) <= n = (reverse (intToChar n)) ++ [ch]
	|otherwise = replicate' ch n

replicate' :: Char -> Int -> [Char]
replicate' _ 0 = []
replicate' ch n = ch : replicate' ch (n-1)
  
intToChar :: Int -> [Char]
intToChar 0 = [] 
intToChar n = chr((rem n 10) + ord '0') : intToChar (div n 10)



