import Data.Char

main :: IO()
main = do
    print $ ordered t1 == True
    print $ ordered t2 == False

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

t1 :: BTree (Int, Int)
t1 = Node (3,10) (Node (5,8) (Node (6,7) Nil Nil) (Node (4,9) Nil Nil)) (Node (2,12) Nil (Node (1,15) Nil Nil))

t2 :: BTree (Int, Int) 
t2 = Node (3,10) (Node (5,8) (Node (6,7) Nil Nil) (Node (7,9) Nil Nil)) (Node (2,12) Nil (Node (1,15) Nil Nil))

ordered :: BTree (Int, Int) -> Bool
ordered Nil = True
ordered tree = helper $ treeToList tree
 where 
    helper [x] = True
    helper (x:xs)
     | fst x <= fst (head xs) && snd x >= snd (head xs) = helper xs
     | otherwise = False

treeToList :: BTree (Int, Int) -> [(Int,Int)]
treeToList Nil = []
treeToList (Node val l r) = (treeToList r) ++ [val] ++ (treeToList l)