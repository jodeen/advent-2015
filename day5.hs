import Data.List
import Data.Maybe
import DayData(day5)

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _ = False

threeVowels :: String -> Bool
threeVowels s = (length $ (filter isVowel s)) >= 3


doubleLetter :: String -> Bool
doubleLetter s = isJust $ find (\(x,y) -> x==y) (zip s (tail s))

isBadString :: (Char, Char) -> Bool
isBadString ('a', 'b') = True
isBadString ('c', 'd') = True
isBadString ('p', 'q') = True
isBadString ('x', 'y') = True
isBadString _ = False

noBadString :: String -> Bool
noBadString s = isNothing $ find isBadString (zip s (tail s))

isNice :: String -> Bool
isNice s = threeVowels s && doubleLetter s && noBadString s

countNice :: [String] -> Int
countNice items = length $ filter isNice items

part1 = countNice day5


pairRepeat (a:b:rest) = isInfixOf [a,b] rest
pairRepeat _ = False

hasPairRepeat s = isJust (find pairRepeat (tails s))

oneRepeat (a:_:c:_) = a == c
oneRepeat _ = False

hasOneRepeat s = isJust (find oneRepeat (tails s))

isNice2 s = hasPairRepeat s && hasOneRepeat s

part2 = length $ filter isNice2 day5