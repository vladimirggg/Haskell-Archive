main :: IO()
main = do
    print $ levelSum numberBTree 1 == 11 -- (5 + 6)
    print $ cone numberBTree == True
 
data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

numberBTree :: BTree Int
numberBTree = (Node 10 (Node 5 (Node 1 Nil Nil) (Node 9 Nil Nil)) (Node 6 (Node 8 Nil Nil) (Node 7 Nil Nil)))

height :: BTree a -> Int
height Nil = 0
height (Node a l r) = max (1 + height l) (1 + height r)

levelSum :: BTree Int -> Int -> Int
levelSum (Node val _ _) 0 = val
levelSum Nil _ = 0
levelSum (Node val l r) lvl = (levelSum l (lvl - 1)) + (levelSum r (lvl - 1))

cone :: BTree Int -> Bool
cone tree = helper tree ((height tree) - 1)
 where
    helper Nil _ = True
    helper tree lvl 
     | lvl == 1 = levelSum tree lvl > levelSum tree (lvl - 1)
     | levelSum tree lvl < levelSum tree (lvl - 1) = False
     | otherwise = helper tree (lvl - 1)