import Data.Char
import Data.List

tfidf :: String -> String -> [String] -> Float
tfidf str sentance text = (tf str sentance) * (idf str text)
	
tf :: String -> String -> Float 
tf [] _ = 0 -- 1
tf _ [] = 0 -- 1
tf str sentance = find' str (dictionary sentance)

find' :: String -> [(String, Float)] -> Float
find' _ [] = 0
find' str (x:xs) 
	| fst x == str =  (snd x)
	| otherwise = find' str xs

idf :: String -> [String] -> Float
idf [] _ = 0
idf str text = logBase 2 (numDocs / numOccurences str text)
	where numDocs = length' text
	
length' :: [String] -> Float
length' [] = 0
length' (x:xs) = 1 + length' xs
	
numOccurences :: String -> [String] -> Float
numOccurences [] _ = 0
numOccurences _ [] = 0
numOccurences str (x:xs) 
	| (occurences str $words x) >= 1 = 1 + numOccurences str xs
	| otherwise = numOccurences str xs

dictionary :: String -> [(String, Float)]
dictionary str = nub $zip tokens $map (\x -> occurences x tokens) tokens 
	where tokens = toLower' $words str

occurences :: String -> [String] -> Float
occurences _ [] = 0
occurences [] _ = 0
occurences str (x:xs) 
	| x == str = 1 + occurences str xs
	| otherwise = occurences str xs
	
toLower' :: [String] -> [String]
toLower' strList = map (\x -> map toLower x) strList

-- returnN :: Int -> Float
-- returnN n = n