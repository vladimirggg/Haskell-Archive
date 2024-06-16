main::IO()
main = do
    print $ getPrimesLC 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 100 1 == [7,17,37,47,67,71,73,79,97]

    print $ getPrimesHOF 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesHOF 100 1 == [7,17,37,47,67,71,73,79,97]

getPrimesLC :: Int -> Int -> [Int]
getPrimesLC x y = [n | n <- [min x y .. max x y], n>1 && null[d | d <- [2..n-1], mod n d == 0] && elem '7' (show n)]

getPrimesHOF ::Int -> Int -> [Int]
getPrimesHOF x y = filter (\ x -> elem '7' (show x)) [n | n <- [min x y .. max x y], n>1 && null[d | d <- [2..n-1], mod n d == 0]]