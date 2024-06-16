main :: IO()
main = do
    print $ countOccurences 121 1 == 2
    print $ countOccurences 222 1 == 0

countOccurences :: Int -> Int -> Int
countOccurences num dig
 | num < 10 && num == dig = 1
 | num < 10 && num /= dig = 0
 | mod num 10 == dig = 1 + countOccurences (div num 10) dig
 | otherwise = countOccurences (div num 10) dig