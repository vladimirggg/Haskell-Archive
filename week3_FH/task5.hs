main :: IO()
main = do
    print $ p 1 == 1
    print $ p 2 == 5
    print $ p 3 == 12
    print $ p 4 == 22
    print $ p 5 == 35
    print $ p 6 == 51

p :: Int -> Int
p x = helper x 0
    where
        helper k num
         | num /= 0 = num
         | k /= 0 = helper k ((k - 1) * k + div (k * (k + 1))  2)