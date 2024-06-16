main :: IO()
main = do 
    print $ rev 1 == 1
    print $ rev 123 == 321
    print $ rev 987654321 == 123456789


numDigits :: Int -> Int
numDigits 0 = 0
numDigits x = 1 + numDigits (div x 10)

rev :: Int -> Int
rev n 
 | n < 10 = n
 |  otherwise = (mod n 10) * (10^(numDigits (div n 10))) + rev (div n 10)
  