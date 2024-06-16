import Data.List
main :: IO()
main = do
    print $ findUncles t 5 == [3,4]
    print $ findUncles t 7 == [2,4]
    print $ findUncles t 10 == [5]
    
type Tree = [(Int, [Int])]

t :: Tree
t = [(1,[2,3,4]),(2,[5,6]),(3,[7]),(4,[8,9]),(5,[]),(6,[10]),(7,[]),(8,[]),(9,[]),(10,[])]

findUncles :: Tree -> Int -> [Int]
findUncles tree node = let
    parentNode = findParent tree node
    in case parentNode of
        [] -> []  -- Node has no parent, return empty list
        [parent] -> delete parent (uncles parent)  -- Node has a single parent, find and return its uncles
  where
    uncles parent = siblings (findParent tree parent)  -- Find uncles of a given parent
    siblings parents = concatMap (snd . flip findNode tree) parents  -- Retrieve siblings for each parent
    findParent t n = [parent | (parent, children) <- t, elem n children]
    findNode key = head . filter (\(k, _) -> k == key)