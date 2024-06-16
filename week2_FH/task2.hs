main :: IO()
main = do
    print $ sumDigitsIter 12345 == 15
    print $ sumDigitsIter 123 == 6

sumDigitsIter :: Int -> Int
sumDigitsIter n = helper n 0
 where 
    helper 0 sum = sum
    helper leftover sum = helper (div leftover 10) (sum + mod leftover 10)