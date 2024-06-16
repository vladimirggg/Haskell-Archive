main :: IO ()
main = do
    print $ getAreas [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == [78.53981633974483,11.25,113.30000000000001,9.127927385194024,6283.185307179587] -- отново го приравнява по различен начин при последния случай - 6283.185307179586
    print $ maxArea [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == Cylinder 20.0 30.0

data Shape a = Circle a | Triangle a a a | Rectangle a a | Cylinder a a
 deriving (Eq)

area :: Floating a => Shape a -> a
area (Circle radius) = pi * radius * radius
area (Triangle a b c) =
  let s = (a + b + c) / 2
   in sqrt (s * (s - a) * (s - b) * (s - c))
area (Rectangle height width) = height * width
area (Cylinder radius height) = 2 * pi * radius * (radius + height)


getAreas :: Floating a => [Shape a] -> [a]
getAreas shapes = map (\ s -> area s) shapes

maxArea :: (Floating a, Ord a) => [Shape a] -> Shape a
maxArea shapes = foldl1 (\acc x -> if area x > area acc then x else acc) shapes