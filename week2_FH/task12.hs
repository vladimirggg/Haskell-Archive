main :: IO()
main = do
    print $ findSum 0 2 10 == 3578 -- 510 + 1022 + 2046
    print $ findSum 5 3 5 == 174 -- 26 + 50 + 98

findSum :: Int -> Int -> Int -> Int
findSum a b n = helper (n - 1) + helper (n - 2) + helper (n - 3)
 where 
    helper 0 = a + b
    helper pow = (2^pow * b ) + helper (pow - 1) 