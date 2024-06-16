main :: IO()
main = do
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True


sumDivs :: Int -> Int
sumDivs n = sum n 0 
    where 
        sum 0 s = s
        sum 1 s = s
        sum leftover s
         | mod n leftover == 0 = sum (leftover - 1) (s + leftover)
         | otherwise = sum (leftover - 1) s

areAmicable :: Int -> Int -> Bool
areAmicable num1 num2 = sumDivs num1 == sumDivs num2
