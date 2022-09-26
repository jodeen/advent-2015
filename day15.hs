import Data.List

sampleData =
  [ [-1, -2, 6, 3, 8],
    [2, 3, -2, -1, 3]
  ] ::
    [[Int]]

day10Data =
  [ [3, 0, 0, -3, 2],
    [-3, 3, 0, 0, 9],
    [-1, 0, 4, 0, 1],
    [0, 0, -2, 2, 8]
  ]

  

allCombos = filter (\c -> 100 == sum c) [[a, b, c, d] | a <- [0 .. 100], b <- [0 .. 100], c <- [0 .. 100], d <- [0 .. 100]]

allCombos2 = filter (\c -> 100 == sum c) [[a, b] | a <- [0 .. 100], b <- [0 .. 100]]  :: [[Int]]

prod :: Num a => (a, [a]) -> [a]
prod (n, l) = map (* n) l

matSum :: [[Int]] -> [Int]
matSum items = map (max 0) (map sum (transpose items))

matProd :: [Int] -> [[Int]] -> [[Int]]
matProd val items = map prod (zip val items)


allRecepies ing combos = map (\amount -> matSum (matProd amount ing)) combos

findMax ing combos = maximum (map (product . take 4) (allRecepies ing combos))
findMax2 ing combos = maximum (map (product . take 4) (filter (\[_,_,_,_,a] -> a == 500) (allRecepies ing combos)))

part1 = findMax day10Data allCombos

part2 = findMax2 day10Data allCombos


