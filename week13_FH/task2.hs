main :: IO()
main = do
    print $ convert tree == (Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil))))

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

tree :: BTree Int
tree = Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil)))

getNodes :: BTree a -> [a]
getNodes Nil = []
getNodes (Node val l r) = val : getNodes l ++ getNodes r

convert :: BTree Int -> BTree Int
convert tree = helper tree (getNodes tree)
 where
    helper Nil _ = Nil
    helper (Node val l r) listOfNodes = Node (sum $ filter (>= val) listOfNodes) (helper l listOfNodes) (helper r listOfNodes)

