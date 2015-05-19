import Data.Char
import Data.List

dictionary :: String -> [(String, Int)]
dictionary str = nub $zip tokens $map (\x -> occurences x tokens) tokens 
	where tokens = toLower' $words str

occurences :: String -> [String] -> Int
occurences _ [] = 0
occurences [] _ = 0
occurences str (x:xs) 
	| x == str = 1 + occurences str xs
	| otherwise = occurences str xs

toLower' :: [String] -> [String]
toLower' strList = map (\x -> map toLower x) strList

