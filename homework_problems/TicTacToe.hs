import Data.Char

nextStates:: [[String]] -> String -> [[[String]]]

nextStates [] _ = []
nextStates board player = nextStatesHelper board board player 0 0 

nextStatesHelper :: [[String]] -> [[String]] -> String -> Int -> Int -> [[[String]]]
nextStatesHelper _ [] _ _ _ = []
nextStatesHelper board (x:xs) player i j 
	| j >= 3 = nextStatesHelper board xs player (i+1) 0
	| -1 /= ind = generateBoard board i ind player ++ nextStatesHelper board (x:xs) player i (ind+1)
	| otherwise = nextStatesHelper board xs player (i+1) 0
	where ind = indexOf x "-" j

indexOf :: [String] -> String -> Int -> Int
indexOf [] _ _ = -1
indexOf row str j = indexOfHelper row str j 0

indexOfHelper :: [String] -> String -> Int -> Int -> Int
indexOfHelper [] _ _ _ = -1
indexOfHelper (x:xs) str j ind 
	| ind < j = indexOfHelper xs str j (ind + 1)
	| x == str = ind
	| otherwise = indexOfHelper xs str j (ind + 1)

generateBoard :: [[String]] -> Int -> Int -> String -> [[[String]]]
generateBoard [] _ _ _ = []
generateBoard board i j player
	| (i-1) >= 0 && (j-1) >= 0 && (findElem board (i-1) (j-1) player) = [placePlayer player board i j] -- ++ generateBoard board (i+1) (j-1) player 
	| (i+1) < 3 && (j-1) >= 0 && (findElem board (i+1) (j-1) player) = [placePlayer player board i j] -- ++ generateBoard board (i-1) (j+1) player
 	| (i-1) >= 0 && (j+1) < 3 && (findElem board (i-1) (j+1) player) = [placePlayer player board i j] -- ++ generateBoard board (i+1) (j+1) player
	| (i+1) < 3 && (j+1) < 3 && (findElem board (i+1) (j+1) player) = [placePlayer player board i j] -- ++ generateBoard board (-1) (-1) player
	| otherwise = []
	
findElem :: [[String]] -> Int -> Int -> String -> Bool
findElem [] _ _ _ = False
findElem board i j player 
	| (nth (nth board i) j) == player = True
	| otherwise = False

nth :: [a] -> Int -> a
nth (x:xs) 0 = x
nth (x:xs) n = nth xs ( n-1)

placePlayer :: String -> [[String]] -> Int -> Int -> [[String]]
placePlayer _ [] _ _  = []
placePlayer player (x:xs) i j 
	| i == 0 = placePlayerInLine x player j : xs  
	| otherwise = x : placePlayer player xs (i-1) j

placePlayerInLine :: [String] -> String -> Int -> [String]
placePlayerInLine [] _ _ = []
placePlayerInLine (x:xs) player j 
	| j == 0 = player : xs
	| otherwise = x : placePlayerInLine xs player (j-1)
	
	
board = [["-", "X", "O"], 
		 ["O", "X", "X"],
		 ["O", "-", "O"]]

board2 = [["O", "X", "O"],
		  ["X", "X", "O"], 
		  ["X", "O", "O"]]
	
board3 = [["-", "-", "O"],
		  ["X", "-", "O"], 
		  ["X", "O", "O"]]
	
	
	
	
	
	
	
	
	
	

-- ??? 
--dublicatePrimes :: [Int]
--dublicatePrimes = foldr func [] naturals
	--where func x acc
		-- | isPrime x = [x, x] ++ acc
		-- | otherwise = acc --- ++ [x]


	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	