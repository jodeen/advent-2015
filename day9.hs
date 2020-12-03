import DayData(day9Data)
import Data.List

sampleData :: [([Char], [Char], Int)]
sampleData = [
    ("London", "Dublin", 464),
    ("London", "Belfast", 518),
    ("Dublin", "Belfast", 141)]

dist (x,y,d) = d

allPlaces pairs = nub ((map (\(x,_,_) -> x) pairs) ++ (map (\(_,y,_) -> y) pairs))

steps path = zip path (tail path)

findDistance :: [(String,String,Int)] -> (String,String) -> Int
findDistance d (s,e) = dist (head (filter (\(x,y,l) -> (x == s && y == e) || (y == s && x == e)) d))

pathDistance ::  [(String,String,Int)] -> [String] -> Int
pathDistance d p = sum (map (findDistance d) (steps p))

part1 :: [([Char], [Char], Int)] -> Int
part1 d = minimum (map (pathDistance d)  allPaths)
    where
        all = allPlaces d
        allPaths = permutations all

part2 :: [([Char], [Char], Int)] -> Int
part2 d = maximum (map (pathDistance d)  allPaths)
    where
        all = allPlaces d
        allPaths = permutations all        