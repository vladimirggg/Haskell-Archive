import Data.Char

main :: IO()
main = do
    print $ minDepthGreenNode colorTree == 2


data Color = Red | Green | Blue
data Tree = Empty | Node Color Tree Tree
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty)(Node Red Empty Empty)) Empty)

minDepthGreenNode :: Tree -> Int
minDepthGreenNode tree
 | getDepth tree == maxIntVal = error "No green Node in the tree!"
 | otherwise = getDepth tree
 where
    getDepth Empty = maxIntVal
    getDepth (Node Green _ _) = 0
    getDepth (Node col l r) = min (1 + getDepth l) (1 + getDepth r)
    maxIntVal = 92233720368547758