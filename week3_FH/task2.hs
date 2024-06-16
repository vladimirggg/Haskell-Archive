main :: IO()
main = do
    print $ sortN 1714 == 7411
    print $ sortN 123450 == 543210
    print $ sortN 123405 == 543210
    print $ sortN 123045 == 543210
    print $ sortN 120345 == 543210
    print $ sortN 102345 == 543210
    print $ sortN 8910 == 9810
    print $ sortN 321 == 321
    print $ sortN 29210 == 92210
    print $ sortN 1230 == 3210
    print $ sortN 55345 == 55543
    print $ sortN 14752 == 75421
    print $ sortN 329450 == 954320
    print $ sortN 9125 == 9521

removeFistOccurrence :: Int -> Int -> Int
removeFistOccurrence n k = helper n 0 0
    where helper num pow passed
            | mod num 10 == k = (div num 10) * (10 ^ pow) + passed
            | otherwise = helper (div num 10) (pow + 1) ((mod num 10) * (10^pow) + passed)

sortN :: Int -> Int
sortN 0 = 0
sortN num = findSmallest num 9 + (sortN $ removeFistOccurrence num findSmallest num 9) * 10
 where
    findSmallest 0 smallest = smallest
    findSmallest n smallest
     | mod n 10 < smallest = findSmallest (div n 10) (mod n 10)
     | otherwise = findSmallest (div n 10) smallest
