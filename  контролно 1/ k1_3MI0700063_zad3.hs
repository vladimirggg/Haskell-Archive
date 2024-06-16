main :: IO()
main = do
    print $ numContentChildren [1, 2, 3] [1, 1] == 1
    print $ numContentChildren [1, 2] [1, 2, 3] == 2
    print $ numContentChildren [1 .. 5] [1, 2, 3] == 3

numContentChildren :: [Int] -> [Int] -> Int
numContentChildren gs ss
 | null gs || null ss = 0
 | head ss >= head gs = 1 + numContentChildren (tail gs) (tail ss)
 | otherwise = numContentChildren (tail gs) (tail ss)