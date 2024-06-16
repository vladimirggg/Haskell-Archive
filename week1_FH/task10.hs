main :: IO()
main = do 
    print $ finalGrade 3 4 4 4.25 4.50 3.75 4.25 5 4.25 == 4.34
    print $ finalGrade 6 6 6 4.50 5 4.50 4.75 5 4.75    == 4.95
    print $ finalGrade 6 0 4 6 6 5 4.75 6 4.75          == 5.14
    print $ finalGrade 4.25 0 3 2 0 0 0 0 0             == 2
    print $ finalGrade 5.50 6 6 6 5.50 5.25 4 5.50 4    == 5.05
    print $ finalGrade 6 6 6 5.50 5.50 4 5 5.50 5       == 5.25
    print $ finalGrade 6 6 6 5.25 6 4 4 5.63 3.50       == 4.84

finalGrade :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float 
finalGrade d1 d2 d3 kz1 kz2 kt1 kt2 iz it
 | total < 3 = 2
 | otherwise = fromIntegral (round $ total * 100) / 100
   where
      d = (d1 + d2 + d3) / 3
      kt = (kt1 + kt2) / 2
      kz = (kz1 + kz2) / 2
      tk = (d / 4) + (kt * 3) / 8 + (kz * 3) / 8
      total = tk / 2 + it / 4 + iz / 4