main :: IO()
main = do
    print $ sumDivisibleNumbers 0 10 5 == 5
    print $ sumDivisibleNumbers 0 100 5 == 990
    print $ sumDivisibleNumbers 100 0 5 == 990

digSum :: Int -> Int
digSum 0 = 0
digSum n = mod n 10 + digSum (div n 10)

sumDivisibleNumbers:: Int -> Int -> Int -> Int
sumDivisibleNumbers start finish k = helper (min start finish) (max start finish)
 where
    helper st end 
     | st == end = 0
     | mod (digSum st) k == 0 = st + helper (st + 1) end
     | otherwise = helper (st + 1) end
        