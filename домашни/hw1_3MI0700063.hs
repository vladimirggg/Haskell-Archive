import Data.List
main::IO()
main = do
    print $ eqSumPowDig 100 2 == 0 
    print $ eqSumPowDig 1000 2 == 0 
    print $ eqSumPowDig 2000 2 == 0 
    print $ eqSumPowDig 200 3 == 153 
    print $ eqSumPowDig 370 3 == 523 
    print $ eqSumPowDig 370 3 == 523 
    print $ eqSumPowDig 400 3 == 894 
    print $ eqSumPowDig 500 3 == 1301 
    print $ eqSumPowDig 1000 3 == 1301 
    print $ eqSumPowDig 1500 3 == 1301


    print $ getNthSevenlikeNum 1 == 1                        
    print $ getNthSevenlikeNum 2 == 7
    print $ getNthSevenlikeNum 3 == 8
    print $ getNthSevenlikeNum 4 == 49
    print $ getNthSevenlikeNum 5 == 50
    print $ getNthSevenlikeNum 6 == 56
    print $ getNthSevenlikeNum 7 == 57
    print $ getNthSevenlikeNum 8 == 343
    print $ getNthSevenlikeNum 1000 


-- TASK 1
eqSumPowDig :: Int -> Int -> Int
eqSumPowDig 1 pow = 0
eqSumPowDig hMax pow
 | hMax == helper hMax = hMax + eqSumPowDig (hMax - 1) pow
 | otherwise = eqSumPowDig (hMax - 1) pow
 where
    helper 0 = 0
    helper num = mod num 10^pow + helper (div num 10)

-- TASK 2
getNthSevenlikeNum :: Int -> Int
getNthSevenlikeNum num = (nub $ sort [d | k <- [1..num], xs <- combinations k [7^d | d <- [0..div num 2]], let d = sum xs]) !! (num - 1)

combinations :: Int -> [Int] -> [[Int]]
combinations count xs = filter ((count==).length) $ subsequences xs