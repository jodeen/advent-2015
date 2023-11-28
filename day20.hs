import Data.List(find)

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

simpleFactor :: Int -> [Int]
simpleFactor i = filter (\x -> (i `mod` x) == 0) [1..i]

-- part1 :: Int -> [Int]
part1 limit = find (\x -> (snd x) >= limit) (map (\x -> (x, (sum  (simpleFactor x)) * 10)) [1..])