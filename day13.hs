import Day13Data(day13Data)


import Data.List

sampleData = [
    ("Alice", 54, "Bob"),
    ("Alice", -79, "Carol"),
    ("Alice", -2, "David"),
    ("Bob", 83, "Alice"),
    ("Bob", -7, "Carol"),
    ("Bob", -63, "David"),
    ("Carol", -62, "Alice"),
    ("Carol", 60, "Bob"),
    ("Carol", 55, "David"),
    ("David", 46, "Alice"),
    ("David", -7, "Bob"),
    ("David", 41, "Carol")] :: [(String, Int, String)]

names d = nub (map (\(a,_,_) -> a) d)

names2 d = "You" : names d

findPair :: [(String, Int, String)] -> (String, String) -> Int
findPair d ("You",b) = 0
findPair d (a, "You") = 0
findPair d (a,b) = h + c
    where 
        Just (_, h, _) = find (\(f,_,l) -> f == a && l == b) d
        Just (_, c, _) = find (\(f,_,l) -> f == b && l == a) d


totalHappiness d seating = happiness
    where
        pairs = (last seating, head seating) : (zip seating (tail seating))
        happiness = sum (map (findPair d) pairs)
        

part1 d = maximum (map (totalHappiness d) (permutations (names d)))
part2 d = maximum (map (totalHappiness d) (permutations (names2 d)))