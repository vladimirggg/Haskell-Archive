main :: IO()
main = do
    print $ persistence 39  == (3,[27,14,4])      --3*9=27, 2*7=14, 1*4=4
    print $ persistence 999 == (4,[729,126,12,2]) --9*9*9=729, 7*2*9=126,  
    print $ persistence 126 == (2,[12,2])         --1*2*6=12, 1*2=2
    print $ persistence 4   == (1,[4])

persistence :: Int -> (Int, [Int])
persistence n 

mult :: Int -> Int
mult num = helper num 1
 where
    helper :: Int -> Int -> Int
    helper num res
     | num < 10 = res * num
     | otherwise = helper (div num 10) res * mod num 10