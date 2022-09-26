
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Category ((>>>))
import Data.Function ((&))
import Day18Data(day18Data)

maxWidth = 99
fullSet = Set.fromList ([(x,y) | x <- [0..maxWidth], y<-[0..maxWidth]])

parseInput :: [String] -> Set (Int, Int)
parseInput rows = (zip rows [0..]) & (concatMap parseLine) & (filter (\(a, _) -> a == '#')) & (map snd) & (Set.fromList)


parseLine :: (String, Int) -> [(Char, (Int, Int))]
parseLine (line, row) = zipWith (\c col -> (c, (row, col))) line [0..]


-- doStep :: Set (Int, Int) -> Set (Int, Int) 

neighbors :: (Int, Int) -> Set (Int, Int)
neighbors (x,y) = Set.fromList [(x-1, y-1), (x-1,y), (x-1, y+1), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y), (x+1,y+1)]

keepOnPoint :: Set (Int, Int) -> (Int, Int) -> Bool
keepOnPoint state point = onNeighbors == 2 || onNeighbors == 3
    where
        onNeighbors = Set.size (Set.intersection state (neighbors point))

turnOnPoint :: Set (Int, Int) -> (Int, Int) -> Bool
turnOnPoint state point = onNeighbors == 3
    where
        onNeighbors = Set.size (Set.intersection state (neighbors point))

doStep :: Set (Int, Int) -> Set (Int, Int)
doStep state = Set.union stillOn newOn 
    where
        stillOn = Set.filter (keepOnPoint state) state
        newOn = Set.filter (turnOnPoint state) fullSet

calcState steps inputString = (parseInput inputString) & (iterate doStep) & (drop steps) & head

part1 = Set.size (calcState 100 day18Data)

turnOnCorners state = Set.union state (Set.fromList [(0,0), (0,maxWidth), (maxWidth,0), (maxWidth, maxWidth)]) 

doStep2 :: Set (Int, Int) -> Set (Int, Int)
doStep2 state = turnOnCorners (Set.union stillOn newOn)
    where
        stuckState = turnOnCorners state
        stillOn = Set.filter (keepOnPoint stuckState) stuckState
        newOn = Set.filter (turnOnPoint stuckState) fullSet

calcState2 steps inputString = (parseInput inputString) & (iterate doStep2) & (drop steps) & head

-- part2 = Set.size (calcState2 5 sampleData)
part2 = Set.size (calcState2 100 day18Data)

sampleData = [".#.#.#",
    "...##.",
    "#....#",
    "..#...",
    "#.#..#",
    "####.."]