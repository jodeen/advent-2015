
import Data.List
import Data.Ord

day14Data = [
    (27, 5, 132),
    (22, 2, 41),
    (11, 5, 48),
    (28, 5, 134),
    (4, 16, 55),
    (14, 3, 38),
    (3, 21, 40),
    (18, 6, 103),
    (18, 5, 84)] :: [(Int, Int, Int)]

kmCycle (speed, dur, _) = speed * dur

cycles totalDur (_, dur, rest) = div totalDur (dur + rest)

dist :: Int -> (Int, Int, Int) -> Int
dist totalDur (speed, dur, rest) = cycles * (speed * dur) + (min dur rem)  * speed
    where     
        (cycles, rem) = divMod totalDur (dur + rest)

part1 d duration = maximum (map (dist duration) d)

distances :: [(Int, Int, Int)] -> Int -> [Int]
distances d dur = map (dist dur) d

points :: [Int] -> [Int]
points dists = map (\d -> if d == m then 1 else 0) dists
    where 
        m = maximum dists 

-- gives you the 1-based number of the reindeer
part2 d duration = maximumBy (comparing fst) (zip totalPoints [1..])
    where
        steps = map (distances d) [1..duration+1]
        finalPoints = map points steps
        byDeer = transpose finalPoints
        totalPoints = map sum byDeer
