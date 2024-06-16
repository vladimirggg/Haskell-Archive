main :: IO()
main = do
    print $ closestAverage [(Temp 1 23.6), (Temp 6 24.2), (Temp 11 24.2), (Temp 16 21.2), (Temp 21 23.8), (Temp 26 26.5), (Temp 31 24.5)]

data Measuring = Temp Int Float

closestAverage :: [Measuring] -> Int
closestAverage temps = fst $ foldr (\ (Temp month temp) (currMonth, currTemp) -> if (abs (avg temps - temp)) < currTemp then (month, (abs (avg temps - temp))) else (currMonth, currTemp)) (0,avg temps) temps


avg :: [Measuring] -> Float
avg temps = sum [monthTemp | (Temp _ monthTemp) <- temps] / fromIntegral (length temps)