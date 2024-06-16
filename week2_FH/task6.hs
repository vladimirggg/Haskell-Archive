main :: IO()
main = do
    print $ isInteresting 410 == True
    print $ isInteresting 212 == False
    print $ isInteresting 567 == False
    print $ isInteresting 70 == True 
    print $ isInteresting 5 == True 
    print $ isInteresting 4 == True
digSum :: Int -> Int
digSum num
 | num < 10 = num
 | otherwise = (mod num 10) + digSum (div num 10)

isInteresting :: Int -> Bool
isInteresting n = mod n (digSum n) == 0