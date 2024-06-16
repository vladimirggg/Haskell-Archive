import Data.List
main :: IO()
main = do
    -- you may get slightly different results eg. [3, 4, 5] on test 1 <- not a problem
    print $ listLeaves [(1, 2, 3), (2, 4, 5)] -- == [4, 3, 5]
    print $ listLeaves [(2, 4, 5), (1, 2, 3)] -- == [4, 5, 3]
    print $ listLeaves [(1, 2, 3), (3, 4, 5), (5, 6, 9)] -- == [2, 4, 6, 9]

listLeaves :: [(Int, Int, Int)] -> [Int]
listLeaves [] = []  -- Base case: empty list, no leaf nodes
listLeaves ((x, y, z):rest)
  | not (hasChild y rest) && not (hasChild z rest) = x : listLeaves rest  -- Current node is a leaf node, add it to the result
  | otherwise = listLeaves rest  -- Current node has children, skip it

-- Helper function to check if a value is present as x in any of the subsequent vectors
hasChild :: Int -> [(Int, Int, Int)] -> Bool
hasChild _ [] = False  -- Base case: empty list, no child
hasChild x ((a, _, _):rest)
  | a == x = True  -- Found a child node
  | otherwise = hasChild x rest  -- Continue checking in the remaining vectors
  
-- НЕ РАБОТИ