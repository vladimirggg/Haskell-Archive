main :: IO()
main = do
    print $ getClosedIntervalRec 1 9 == [1, 2, 3, 4, 5, 6, 7, 8, 9]
    print $ getClosedIntervalRec 9 1 == [1, 2, 3, 4, 5, 6, 7, 8, 9]

    print $ getClosedIntervalOneLine 1 9 == [1, 2, 3, 4, 5, 6, 7, 8, 9]
    print $ getClosedIntervalOneLine 9 1 == [1, 2, 3, 4, 5, 6, 7, 8, 9]

getClosedIntervalRec :: Int -> Int -> [Int]
getClosedIntervalRec x y = helper (min x y) (max x y)
 where
    helper :: Int -> Int -> [Int]
    helper start end 
     | start > end = []
     | otherwise = start : helper (start + 1) end

getClosedIntervalOneLine :: Int -> Int -> [Int]
getClosedIntervalOneLine x y = [min x y .. max x y]