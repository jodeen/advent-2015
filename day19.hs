import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function ((&))
import Data.Maybe 
import Data.List (find,nub)
import Debug.Trace (trace)


doReplacement :: String -> (String, String) -> Maybe String
doReplacement (s:xs) (c:[], replacement) = if (c == s) then Just (replacement ++ xs) else Nothing
doReplacement (s1:s2:xs) (c1:c2:[], replacement)  = if ([c1,c2] == [s1,s2]) then Just (replacement ++ xs) else Nothing
doReplacement s _ = Nothing


replacementStep :: [(String, String)] -> String -> [String]
replacementStep replacements mol =  (map (doReplacement mol) replacements) & catMaybes

indexes :: String -> [(String, String)]
indexes root = map (\i -> splitAt i root) [0..(length root - 1)]

uniqueReplacements :: [(String, String)] -> String -> Set String
uniqueReplacements replacements root = (indexes root) & concatMap (\ (h,rest) -> (replacementStep replacements rest) & map (\s -> h ++ s)) & Set.fromList


sampleReplacements = [("H", "HO"), ("H", "OH"), ("O", "HH")]


part1 replacements root = Set.size (uniqueReplacements replacements root)

splitString :: String -> String -> [(String, String, String)]
splitString molecule atom = map (splitSub molecule (length atom)) ([0..((length molecule) - (length atom))]) 

splitSub :: String -> Int -> Int -> (String, String, String)
splitSub s keyLength idx  = (h, key, rest)
    where
        (h, xs) = splitAt idx s
        (key, rest) = splitAt keyLength xs

doReplacement2 :: String -> (String, String) -> [String]
doReplacement2 s (key, value) = map (\(h,_, t) -> h ++ value ++ t) (filter (\(_, k, _) -> k == key) (splitString s key))

applyReplacements :: [(String, String)] -> String -> [String]
applyReplacements reps molecule = concatMap (doReplacement2 molecule) reps

bfs :: [(String, String)] -> String -> String -> Maybe Int
bfs replacements seed match = loop [seed] 0 where
    loop xs level | any (\x -> x == match) xs = Just level
            | otherwise = loop ((nub (filter (\x -> (length x) < (length seed)) (concatMap (applyReplacements replacements) xs)))) (level+1)
    

sampleReplacements2 = [("e", "H"), ("e", "O"), ("H", "HO"), ("H", "OH"), ("O", "HH")]

part2 = bfs day19Data "e" day19Input

day19Data = [("Al", "ThF"),
            ("Al", "ThRnFAr"),
            ("B", "BCa"),
            ("B", "TiB"),
            ("B", "TiRnFAr"),
            ("Ca", "CaCa"),
            ("Ca", "PB"),
            ("Ca", "PRnFAr"),
            ("Ca", "SiRnFYFAr"),
            ("Ca", "SiRnMgAr"),
            ("Ca", "SiTh"),
            ("F", "CaF"),
            ("F", "PMg"),
            ("F", "SiAl"),
            ("H", "CRnAlAr"),
            ("H", "CRnFYFYFAr"),
            ("H", "CRnFYMgAr"),
            ("H", "CRnMgYFAr"),
            ("H", "HCa"),
            ("H", "NRnFYFAr"),
            ("H", "NRnMgAr"),
            ("H", "NTh"),
            ("H", "OB"),
            ("H", "ORnFAr"),
            ("Mg", "BF"),
            ("Mg", "TiMg"),
            ("N", "CRnFAr"),
            ("N", "HSi"),
            ("O", "CRnFYFAr"),
            ("O", "CRnMgAr"),
            ("O", "HP"),
            ("O", "NRnFAr"),
            ("O", "OTi"),
            ("P", "CaP"),
            ("P", "PTi"),
            ("P", "SiRnFAr"),
            ("Si", "CaSi"),
            ("Th", "ThCa"),
            ("Ti", "BP"),
            ("Ti", "TiTi"),
            ("e", "HF"),
            ("e", "NAl"),
            ("e", "OMg")]

day19Input = "CRnCaSiRnBSiRnFArTiBPTiTiBFArPBCaSiThSiRnTiBPBPMgArCaSiRnTiMgArCaSiThCaSiRnFArRnSiRnFArTiTiBFArCaCaSiRnSiThCaCaSiRnMgArFYSiRnFYCaFArSiThCaSiThPBPTiMgArCaPRnSiAlArPBCaCaSiRnFYSiThCaRnFArArCaCaSiRnPBSiRnFArMgYCaCaCaCaSiThCaCaSiAlArCaCaSiRnPBSiAlArBCaCaCaCaSiThCaPBSiThPBPBCaSiRnFYFArSiThCaSiRnFArBCaCaSiRnFYFArSiThCaPBSiThCaSiRnPMgArRnFArPTiBCaPRnFArCaCaCaCaSiRnCaCaSiRnFYFArFArBCaSiThFArThSiThSiRnTiRnPMgArFArCaSiThCaPBCaSiRnBFArCaCaPRnCaCaPMgArSiRnFYFArCaSiThRnPBPMgAr"
