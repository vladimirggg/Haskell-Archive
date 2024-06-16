import Data.List
main :: IO()
main = do
    print $ leavesAreEqual t1 t2 == True
    print $ leavesAreEqual t3 t4 == False


data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

t1 :: BTree Int
t1 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil)))

t2 :: BTree Int
t2 = Node 25 (Node 10 (Node 1 Nil Nil) Nil) (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil))

t3 :: BTree Int
t3 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil)))

t4 :: BTree Int
t4 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 27 Nil Nil) (Node 32 Nil Nil)))

getLeaves :: BTree a -> [a]
getLeaves Nil = []
getLeaves (Node val Nil Nil) = [val]
getLeaves (Node val l r) = getLeaves l ++ getLeaves r

leavesAreEqual :: BTree Int -> BTree Int -> Bool
leavesAreEqual tree1 tree2 = (sort $ getLeaves tree1) == (sort $ getLeaves tree2)