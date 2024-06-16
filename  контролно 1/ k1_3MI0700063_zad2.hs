main :: IO()
main = do
    (aroundFib 100) 25 == [('1',3)]
    (aroundFib 180) 25 == [('1',5),('7',3)]
    (aroundFib 1700) 25 == [('1',4),('2',5),('0',6),('4',5),('5',7),('2',4),('6',7),('3',5),('0',4),('8',5),('4',5),('4',4),('7',7),('7',6),('2',2)] 
    (aroundFib 500) 42 == [('0',6),('2',7),('2',6)] 
    (aroundFib 6000) 242 == [('5',31),('8',33),('8',31),('7',35),('7',31),('4',7)]


aroundFib :: Integer -> (Int -> [(Char, Int)])
aroundFib n
 | n < 0 = error "The number must be positive!"
 | otherwise = (\k -> )