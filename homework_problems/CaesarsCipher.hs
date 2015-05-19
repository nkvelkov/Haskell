import Data.Char

caesar_encrypt :: String -> Int -> String
caesar_encrypt [] _ = []
-- caesar_encrypt (x:xs) offset = chr(offset + ord x) : caesar_encrypt xs offset
caesar_encrypt (x:xs) offset 
	| isLowChar x = (chr(ord 'a' + rem (offset + ord x - ord 'a') 26) ) : caesar_encrypt xs offset
	| isUpChar x = (chr(ord 'A' + rem (offset + ord x - ord 'A') 26) ) : caesar_encrypt xs offset
	| otherwise = x : caesar_encrypt xs offset
	
isUpChar :: Char -> Bool
isUpChar ch = pos >= ord 'A' && pos <= ord 'Z'
	where pos = ord ch
	
isLowChar :: Char -> Bool
isLowChar ch = pos >= ord 'a' && pos <= ord 'z'
	where pos = ord ch



decode :: String -> Int -> String
decode [] offset = []
decode (x:xs) offset = chr((ord x) - offset) : decode xs offset

palindrome' :: [Char] -> Bool
palindrome' x = input == (reverse input)
                where input = filter (\x -> x `elem` ['a'..'z']) x