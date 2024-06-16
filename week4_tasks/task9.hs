main :: IO()
main = do
    print $ getPrimesLC 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 100 1 == [7,17,37,47,67,71,73,79,97]

    print $ getPrimesHOF 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesHOF 100 1 == [7,17,37,47,67,71,73,79,97]

getPrimesLC :: Int -> Int -> [Int]
getPrimesLC x y = [d | d <- [min x y .. max x y], d > 1 &&  null [n | n <- [2 .. d-1], mod d n == 0]]