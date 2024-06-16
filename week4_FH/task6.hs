main :: IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392 -- n = 5, d = 2
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n d = sum $ take n [k | k <- [1 .. ], isPrime k && elem (show d) [[c] | c <- show k]]

isPrime :: Int -> Bool
isPrime k = k > 1 && null [m | m <- [2 .. k - 1], mod k m == 0]