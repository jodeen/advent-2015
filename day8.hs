

import System.IO

countChar :: String -> Int
countChar [] = 0
countChar ('\"':xs) = countChar xs
countChar ('\\':'\\':xs) = 1 + countChar xs
countChar ('\\':'\"':xs) = 1 + countChar xs
countChar ('\\':'x':_:_:xs) = 1 + countChar xs
countChar (_:xs) = 1 + countChar xs

countDiff :: String -> Int
countDiff s = (length s) - (countChar s)

encodeCount :: String -> Int
encodeCount [] = 2
encodeCount ('\"':xs) = 2 + encodeCount xs
encodeCount ('\\':xs) = 2 + encodeCount xs
encodeCount (_:xs) = 1 + encodeCount xs

main = do
    withFile "day8.txt" ReadMode (\handle -> do
    contents <- hGetContents handle
    let c = sum (map (\l -> (length l) - (countChar l)) (lines contents))

    putStr (show c)

    let d = sum (map (\l -> (encodeCount l) - (length l)) (lines contents))
    putStr (show d))

