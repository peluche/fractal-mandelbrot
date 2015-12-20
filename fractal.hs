type Point = (Float, Float)

next :: Point -> Point -> Point
next (u, v) (x, y) = (x * x - y * y + u, 2 * x * y + v)

mandelbrot :: Point -> [Point]
mandelbrot p = drop 1 $ iterate (next p) (0, 0)

near_origin :: Point -> Bool
near_origin (u, v) = u * u + v * v < 100

type Pixelify color = Point -> color

choose_color :: [color] -> (Point -> [Point]) -> Pixelify color
choose_color palette f = (palette !!) . length . take n . takeWhile near_origin . f
  where n = length palette - 1

range :: Int -> Float -> Float -> [Float]
range n min max = take n [min, min + (max - min) / fromIntegral (n - 1) ..]

type Grid a = [[a]]

grid :: Int -> Int -> Point -> Point -> Grid Point
grid col row (x, y) (xx, yy) = [[(a, b) | a <- range col x xx] | b <- range row y yy]

convert :: Grid Point -> Pixelify color -> Grid color
convert points f = map (map f) points

colorize = choose_color " .,-+=:;!|/?([{%&$@#" mandelbrot

draw_ascii :: Grid Point -> IO()
draw_ascii gs = putStr $ unlines $ convert gs colorize

fig1 = draw_ascii $ grid 79 37 (-2.25, -1.5) (0.75, 1.5)
