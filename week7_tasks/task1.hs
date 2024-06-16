main :: IO()
main = do
    print $ sumEvenly 1 10 == 41
    print $ sumEvenly 5 20 == 175

sumEvenly :: Int -> Int -> Int
sumEvenly x y = sum $ filter isEvenly [x .. y]

isEvenly :: Int -> Bool
isEvenly num = even $ length [x | x <- [1 .. num], mod num x == 0]