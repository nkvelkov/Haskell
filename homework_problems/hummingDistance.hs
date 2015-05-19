
hummingDistance :: String -> String -> Int
hummingDistance str1 str2 = length [x | (x, y) <- zip str1 str2, x /= y]
