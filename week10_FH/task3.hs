main :: IO()
main = do
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10)] == (4.47213595499958,ThreeD 4.0 5.0 6.0,ThreeD 2.0 5.0 10.0) -- does not print the points in the same order
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10), (ThreeD 5 2 (-10)), (ThreeD (-2) 1 45), (ThreeD 12 0 2)] == (4.47213595499958,ThreeD 4.0 5.0 6.0,ThreeD 2.0 5.0 10.0)
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10), (ThreeD 5 2 (-10)), (ThreeD (-2) 1 45), (ThreeD 12 0 2), (ThreeD 6 5 4)] == (2.8284271247461903,ThreeD 4.0 5.0 6.0,ThreeD 6.0 5.0 4.0)

    print $ getClosestDistance [(TwoD 4 6), (TwoD 5 10), (TwoD 5 29), (TwoD 1 45), (TwoD 0 2), (TwoD 69 42)] == (4.123105625617661,TwoD 4.0 6.0,TwoD 5.0 10.0)

data Point a = TwoD a a | ThreeD a a a
  deriving (Show, Eq, Read, Ord)

distance :: Floating a => Point a -> Point a -> a
distance (TwoD x1 y1) (TwoD x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
distance (ThreeD x1 y1 z1) (ThreeD x2 y2 z2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2)
distance _ _ = error "Points in different dimensions"

getClosestDistance :: (Floating a, Ord a) => [Point a] -> (a, Point a, Point a)
getClosestDistance points =
  let
    pairs = [(p1, p2) | p1 <- points, p2 <- points, p1 /= p2]
    distances = [(distance p1 p2, p1, p2) | (p1, p2) <- pairs]
    (dist, p1, p2) = minimum distances
  in
    (dist, p1, p2)
