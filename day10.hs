import Data.List

doStep :: String -> Int -> String
doStep a _ = intercalate "" (map doSay (group a))

doSay :: String -> String
doSay s = show (length s) ++ [(head s)]

part1 d = length (foldl doStep d [1..40])
part2 d = length (foldl doStep d [1..50])