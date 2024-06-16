main :: IO ()
main = do
  print $ perimeter (Circle 5) == 31.41592653589793
  print $ perimeter (Rectangle 2.5 4.5) == 14
  print $ perimeter (Rectangle 5.5 20.6) == 52.2
  print $ perimeter (Triangle 5.3 3.9 4.89) == 14.09
  print $ perimeter (Cylinder 2.5 10) == 30

  print $ area (Circle 5) == 78.53981633974483
  print $ area (Rectangle 2.5 4.5) == 11.25
  print $ area (Rectangle 5.5 20.6) == 113.30000000000001
  print $ area (Triangle 5.3 3.9 4.89) == 9.127927385194024
  print $ area (Cylinder 20 30) == 6283.185307179587 -- закръгля го по гразличен начин 6283.185307179586

data Shape a = Circle a | Triangle a a a | Rectangle a a | Cylinder a a

perimeter :: Floating a => Shape a -> a
perimeter (Circle radius) = 2 * pi * radius
perimeter (Triangle a b c) = a + b + c
perimeter (Rectangle height width) = 2 * (height + width)
perimeter (Cylinder radius height) = 4 * radius + 2 * height

area :: Floating a => Shape a -> a
area (Circle radius) = pi * radius * radius
area (Triangle a b c) =
  let s = (a + b + c) / 2
   in sqrt (s * (s - a) * (s - b) * (s - c))
area (Rectangle height width) = height * width
area (Cylinder radius height) = 2 * pi * radius * (radius + height)
