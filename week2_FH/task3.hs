main :: IO()
main = do
    print $ sumPrimeDivs 0 == 0
    print $ sumPrimeDivs 6 == 5 -- 2 + 3
    print $ sumPrimeDivs 18 == 5 -- 2 + 3
    print $ sumPrimeDivs 19 == 19
    print $ sumPrimeDivs 45136 == 53

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = helper 2
 where
    helper start
     | start == n = True
     | mod n start == 0 = False
     | otherwise = helper (start + 1)

 
sumPrimeDivs :: Int -> Int
sumPrimeDivs n = sum n 0 
    where 
        sum 0 s = s
        sum 1 s = s
        sum leftover s
         | mod n leftover == 0 && isPrime leftover = sum (leftover - 1) (s + leftover)
         | otherwise = sum (leftover - 1) s