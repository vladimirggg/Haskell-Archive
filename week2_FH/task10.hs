main :: IO()
main = do
    print $ countPalindromes 5 13 == 5 -- 6 7 8 9 11
    print $ countPalindromes 13 5 == 5 -- 6 7 8 9 11

numDigits :: Int -> Int
numDigits x 
 | x == 0 = 0
 | otherwise = 1 + numDigits (div x 10)

rev :: Int -> Int
rev n 
 | n < 10 = n
 | otherwise = (mod n 10) * (10 ^ (numDigits (div n 10))) + rev (div n 10)

      

countPalindromes :: Int -> Int -> Int
countPalindromes start end = helper (min start end + 1) (max start end)
 where 
    helper first last
        | first == last = 0
        | first == rev first = 1 + helper (first + 1) last
        | otherwise = helper (first + 1) last