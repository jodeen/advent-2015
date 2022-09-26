import DayData (day3)
import qualified Data.Set as Set
import Data.List


type Point = (Int, Int)

doMove :: Point -> Char -> Point
doMove (x,y) '>' = (x+1,y)
doMove (x,y) '<' = (x-1,y)
doMove (x,y) '^' = (x, y+1)
doMove (x,y) 'v' = (x, y-1)

isEven :: Int -> Bool
isEven i = (mod i 2) == 0

visitHouses :: [Char] -> [Point]
visitHouses = scanl doMove (0,0) 

part1 :: [Char] -> Int
part1 = Set.size . Set.fromList . visitHouses

split :: [Char] -> ([Char], [Char])
split dir = (map fst a, map fst b)
    where (a,b) = partition (isEven .snd)  (zip dir [1..])


part2 :: [Char] -> Int
part2 dir = Set.size . Set.fromList $ (union (visitHouses santa) (visitHouses robo))
    where
        (santa, robo) = split dir
