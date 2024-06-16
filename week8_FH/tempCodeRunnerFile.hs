area :: Floating a => Shape a -> a
area (Circle radius) = pi * radius * radius
area (Triangle a b c) =
  let s = (a + b + c) / 2
   in sqrt (s * (s - a) * (s - b) * (s - c))
area (Rectangle height width) = height * width
area (Cylinder radius height) = 2 * pi * radius * (radius + height)
