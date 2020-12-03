import Data.Char
import Data.Maybe
import Data.List

toInt :: Char -> Integer
toInt c = toInteger (ord c) - 97

toNum :: String -> Integer
toNum a = sum (map (\(c,p) -> (toInt c) * (26^p)) (zip (reverse a) [0..]) )


toChar :: Integer -> Char
toChar i = chr (fromInteger i + 97)

toString :: Integer -> String
toString 0 = "z"
toString num = (if (d /= 0) then (toString d) else "") ++ [toChar m] 
    where
        (d,m) = divMod num 26

increment a = toString ((toNum a) + 1)

matchesSeq :: String -> Bool
matchesSeq [] = False
matchesSeq [_] = False
matchesSeq [_, _] = False
matchesSeq (a:b:c:r) = if (toInt c) - (toInt b) == 1 && (toInt b) - (toInt a) == 1 then True else matchesSeq ([b,c] ++ r)

matchesLetters :: String -> Bool
matchesLetters pass = isNothing (find (\c -> c == 'i' || c == 'o' || c == 'l') pass)

matchesDoubles :: String -> Bool
matchesDoubles [] = False
matchesDoubles [_] = False
matchesDoubles (a:b:r) = if (a == b) then matchesDoubles2 a r else matchesDoubles (b : r)

matchesDoubles2 :: Char -> String -> Bool
matchesDoubles2 _ [] = False
matchesDoubles2 _ [a] = False
matchesDoubles2 c (a:b:r) =  (a == b) && (a /= c) || (matchesDoubles2 c (b : r))

matchesPass a = (matchesSeq a) && (matchesLetters a) && (matchesDoubles a)

allPasses start = filter matchesPass (iterate increment start)

part1 = head (allPasses "hxbxwxba")

part2 = take 2 (allPasses "hxbxxyzz")