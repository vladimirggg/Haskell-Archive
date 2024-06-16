main :: IO()
main = do
    print $ removeD 1 656 == 656
    print $ removeD 5 656 == 66
    print $ removeD 6 656 == 5
    print $ removeD 0 606 == 66
    print $ removeD 0 600 == 6
    print $ removeD 6 600 == 0
    print $ removeD 2 1234 == 134

rev :: Int -> Int
rev n 
 | n < 10 = n
 |  otherwise = (mod n 10) * (10^(numDigits (div n 10))) + rev (div n 10)
  where
    numDigits 0 = 0
    numDigits x = 1 + numDigits (div x 10)
  
removeD :: Int -> Int -> Int
removeD d n = rev (helper d n 0)
 where 
    helper dig 0 res = div res 10
    helper dig num res
     | mod num 10 == d = helper dig (div num 10) res
     | otherwise = helper dig (div num 10) ((res + mod num 10) * 10)
