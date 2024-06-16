main :: IO()
main = do
    --print $ countDigitsIter (-13) -- error "n was negative"
    print $ countDigitsIter 12345 == 5
    print $ countDigitsIter 123 == 3

    --print $ countDigitsRec (-13) -- error "n was negative"
    print $ countDigitsRec 12345 == 5
    print $ countDigitsRec 123 == 3

countDigitsIter :: Int -> Int
countDigitsIter n = if n < 0 then error "n was negative" else helper n 0
 where 
    helper 0 digits = digits
    helper leftover digits = helper (div leftover 10) (digits + 1)

countDigitsRec :: Int -> Int
countDigitsRec n
 | n < 0 = error "n was negative"
 | n < 10 = 1
 | otherwise = 1 + countDigitsRec (div n 10)