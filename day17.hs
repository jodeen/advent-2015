import Data.Bits

cycled :: [Integer] -> [[Integer]]
cycled items = map (\i -> take (length items) (drop i (cycle items))) [1 .. length items]

testInput = [20, 15, 10, 5, 5]

input = [43, 3, 4, 10, 21, 44, 4, 6, 47, 41, 34, 17, 17, 44, 36, 31, 46, 9, 27, 38]

countCombos :: Integer -> [Integer] -> Integer 
countCombos target [] = 0
countCombos target (x:xs) = if target == x then 1 + (countCombos target xs) else (countCombos (target - x) xs)


getSum :: Integer -> [Integer]  -> Integer 
getSum mask items = sum (map fst (filter (\(item, idx) -> testBit mask idx) (zip items [0..])))

matchingSum :: [Integer] -> Integer -> [Integer]
matchingSum items target = filter (\mask -> (getSum mask items) == target) [0..2^(length items)]

part1 :: [Integer] -> Integer -> Int 
part1 items target = length (matchingSum items target)

part2 :: [Integer] -> Integer -> Int
part2 items target = length (filter (== minVal) pops)
    where
        pops = map popCount (matchingSum items target)
        minVal = minimum pops