
data Answer = Yes Int | No Int | Unknown deriving (Show, Eq)
type Test = [Answer]
type Exam = [Test]

valueOfAnswer :: Answer -> Int
valueOfAnswer (Yes n) = n
valueOfAnswer (No n) = -n
valueOfAnswer (Unknown) = 0

testScore :: Test -> Int
testScore [] = 0
testScore (x:xs) = valueOfAnswer x + testScore xs

averageScore :: Exam -> Double
averageScore [] = 0
averageScore (x:xs) = ((sumElems (x:xs) / fromIntegral (length (x:xs) )))

sumElems :: Exam -> Double
sumElems [] = 0
sumElems (x:xs) = fromIntegral (testScore x) + sumElems xs

aboveLimit :: Int -> Exam -> Int
aboveLimit _ [] = 0
aboveLimit k (x:xs) 
	| k <= (testScore x) = 1 + aboveLimit k xs
	| otherwise = aboveLimit k xs

highestScore :: Exam -> Test
highestScore exam = findTest (maximum (map (\ x -> testScore x) exam) ) exam

findTest :: Int -> [Test] -> Test
findTest k (x:xs)
	| testScore x == k = x
	| otherwise = findTest k xs

exampleTest1 :: Test
exampleTest1 = [Yes 5, No 3, Unknown, Yes 2, No 1, Yes 3, Yes 2, No 1, Unknown]
-- testScore -> 10
exampleTest2 :: Test
exampleTest2 = [Yes 5, Yes 5, Yes 5, No 5, Unknown]
--  averageScore -> 8.5
--  highestScore -> exampleTest2
exampleExam :: Exam
exampleExam = [exampleTest1, exampleTest2]