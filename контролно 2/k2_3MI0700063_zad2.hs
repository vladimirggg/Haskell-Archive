import Data.Char
main :: IO()
main = do
    print $ solve ["abode","ABc","xyzD"] == [4,3,1] 
    print $ solve ["abide","ABc","xyz"] == [4,3,0] 
    print $ solve ["IAMDEFANDJKL","thedefgh","xyzDEFghijabc"] == [6,5,7] 
    print $ solve ["encode","abc","xyzD","ABmD"] == [1,3,1,3]

solve :: [String] -> [Int]
solve (w:[]) = (inspectWord w 0) : []
solve (w:ws) = (inspectWord w 0) : solve ws

inspectWord :: [Char] -> Int -> Int
inspectWord [] _ = 0
inspectWord (ch:chs) i
 | (ord $ toLower ch) == 97 + i = 1 + inspectWord chs (i + 1)
 | otherwise = inspectWord chs (i + 1)
