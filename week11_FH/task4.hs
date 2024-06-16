import Data.Char

main :: IO()
main = do
    print $ isGraceful t1 == True
    print $ isGraceful t2 == False

data NTree a = Nil | Node a [(NTree a)]
 deriving (Show, Eq)

t1 :: NTree Int
t1 = Node 1 [(Node 3 []), (Node 5 []), (Node 7 []), (Node 9 [])]

t2 :: NTree Int
t2 = Node 7 [(Node 9 [(Node 5 []), (Node 2 [])])]

isGraceful :: NTree Int -> Bool
isGraceful Nil = True
isGraceful (Node _ []) = True
isGraceful (Node val tree) = all (\ (Node nextVal _) -> even $ abs (val - nextVal)) tree && all isGraceful tree