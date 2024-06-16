import Data.List
main :: IO()
main = do
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 0 1 == [[1]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 1 == [[1, 2], [1, 3]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 2 1 == [[1, 2, 3], [1, 2, 4]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 2 == [[2,3],[2,4]]

type Node = Int
type Graph = [(Node, [Node])]
type Path = [Node]

simplePaths :: Graph -> Int -> Node -> [Path]
simplePaths graph k n
    | not (any (\(node, _) -> node == n) graph) = error "Node not present in the graph!"
    | k == 0 = [[n]]
    | otherwise = [ n : p | neighbor <- neighbors, p <- simplePaths graph (k-1) neighbor ]
    where
        neighbors = [ neighbor | (node, adjList) <- graph, node == n, neighbor <- adjList ]
