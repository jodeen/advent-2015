import DayData (day2)


surfaceArea :: (Int, Int, Int) -> Int
surfaceArea (l, w, h) = 2*l*w + 2*w*h + 2*h*l

paperNeed :: (Int, Int, Int) -> Int
paperNeed (l, w, h) = surfaceArea(l,w,h) + (minimum [l*w, w*h, h*l])

part1 = sum (map paperNeed day2)

perimeter :: Int -> Int -> Int 
perimeter x y = 2 * (x+y)

cubicFeet (l,w,h) = l * w * h

ribbonDist :: (Int, Int, Int) -> Int
ribbonDist (l, w, h) = minimum [perimeter l w, perimeter l h, perimeter w h]

ribbonNeed p = (ribbonDist p) + (cubicFeet p)

part2 = sum (map ribbonNeed day2)