import Data.Char

main :: IO()
main = do
    print $ maxDepthBlueNode colorTree == 2


data Color = Red | Green | Blue
data Tree = Empty | Node Color Tree Tree
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty)(Node Red Empty Empty)) Empty)

maxDepthBlueNode :: Tree -> Int
maxDepthBlueNode tree 
 | result == -1 = error "No Blue node in the tree!" 
 | otherwise = result
  where
    result = helper tree 0 (-1)
    helper Empty _ res = res
    helper (Node Blue left right) depth res = max (helper left (depth + 1) (max depth res)) (helper right (depth + 1) (max depth res))
    helper (Node col left right) depth res = max (helper left (depth + 1) res) (helper right (depth + 1) res)