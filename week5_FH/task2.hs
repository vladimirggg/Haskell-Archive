main ::IO()
main = do
    -- print $ getPalindromes 132465 == 8
    -- print $ getPalindromes 654546 == 8
    -- print $ getPalindromes 100001 == 100012
    -- print $ getPalindromes 21612 == 21614
    -- print $ getPalindromes 26362 == 26364
    print $ (\x y z -> z - y == y - x) [3,5,7]
-- getPalindromes :: Int -> Int

