main :: IO()
main = do
    print $ removeFistOccurrence 15365 5 == 1536
    print $ removeFistOccurrence 15360 0 == 1536
    print $ removeFistOccurrence 15300 0 == 1530
    print $ removeFistOccurrence 15365 1 == 5365
    print $ removeFistOccurrence 35365 3 == 3565
    print $ removeFistOccurrence 1212 1 == 122
    print $ removeFistOccurrence 1212 2 == 121
    print $ removeFistOccurrence (removeFistOccurrence 1212 1) 1 == 22


removeFistOccurrence :: Int -> Int -> Int
removeFistOccurrence n k = helper n 0 0
 where 
    helper 0 pow passed = passed
    helper num pow passed
     | mod num 10 == k = (div num 10) * (10 ^ pow) + passed
     | otherwise = helper (div num 10) (pow + 1) (mod num 10 * 10^pow + passed)