{-# LANGUAGE DeriveGeneric, TupleSections, LambdaCase #-}

-- implemented a simple JSON parser just to get a feel for parsing in a functional language
-- mainly copied from https://abhinavsarkar.net/posts/json-parsing-from-scratch-in-haskell/#parser

import GHC.Generics (Generic)
import Data.List (intercalate, find)
import Data.Functor (($>))
import Data.Char
import Control.Applicative (Alternative(..), optional)



import Day12Data(day12Data)
import Data.Maybe (isJust)


data JValue = JNull
    | JBool Bool
    | JString String
    | JInt Int
    | JArray [JValue]
    | JObject [(String, JValue)]
    deriving (Eq)

instance Show JValue where
    show value = case value of
        JNull -> "null"
        JBool True -> "true"
        JBool False -> "false"
        JString s -> s
        JInt s -> show s
        JArray a -> "[" ++ intercalate ", " (map show a) ++ "]"
        JObject o -> "{" ++ intercalate ", " (map showKv o) ++"}"
        where
        showKv (k,v) = k ++ ": " ++ show v

newtype Parser i o = Parser { runParser :: i -> Maybe (i,o)}

satsify :: (a -> Bool) -> Parser [a] a
satsify predicate = Parser $ \case
    (x:xs) | predicate x -> Just (xs, x)
    _                    -> Nothing

char :: Char -> Parser String Char
char c = satsify (== c)

instance Functor (Parser i) where
    fmap f parser = Parser $ fmap (fmap f) . runParser parser

digit :: Parser String Int
digit = digitToInt <$> satsify isDigit


instance Applicative (Parser i) where
    pure x = Parser $ pure . (, x)
    pf <*> po = Parser $ \input -> case runParser pf input of
        Nothing -> Nothing
        Just (rest, f) -> fmap f <$> runParser po rest

string :: String -> Parser String String
string "" = pure ""
string (c:cs) = (:) <$> char c <*> string cs

jNull :: Parser String JValue
jNull = string "null" $> JNull

instance Alternative (Parser i) where
    empty = Parser $ const empty
    p1 <|> p2 = Parser $ \input -> runParser p1 input <|> runParser p2 input

jBool :: Parser String JValue
jBool = string "true" $> JBool True
    <|> string "false" $> JBool False

jsonChar :: Parser String Char
jsonChar = satsify (\c -> not (c == '\"' || c == '\\' || isControl c))

digitsToNumber :: Int -> Int -> [Int] -> Int
digitsToNumber base = foldl (\num d -> num * fromIntegral base + fromIntegral d)

instance Monad (Parser i) where
    p >>= f = Parser $ \input -> case runParser p input of
        Nothing -> Nothing
        Just (rest, o) -> runParser (f o) rest

jString :: Parser String JValue
jString = JString <$> (char '"' *> jString')
    where 
        jString' = do
            optFirst <- optional jsonChar
            case optFirst of
                Nothing -> "" <$ char '"'
                Just first -> (first:) <$> jString'

jUInt :: Parser String Int
jUInt = (\d ds -> digitsToNumber 10 0 (d:ds)) <$> digit19 <*> digits
    <|> fromIntegral <$> digit

digit19 :: Parser String Int
digit19 = digitToInt <$> satsify (\x -> isDigit x && x/='0')

digits ::  Parser String [Int]
digits = some digit

signInt :: Maybe Char -> Int -> Int
signInt (Just '-') i = negate i
signInt _ i = i

jInt :: Parser String JValue
jInt = JInt <$> (signInt <$> optional (char '-') <*> jUInt)

surroundedBy :: Parser String a -> Parser String b -> Parser String a
surroundedBy p1 p2 = p2 *> p1 <* p2

separatedBy :: Parser i v -> Parser i s -> Parser i [v]
separatedBy v s = (:) <$> v <*> many (s *> v) <|> pure []

spaces :: Parser String String
spaces = many (char ' ' <|> char '\n' <|> char '\r' <|> char '\t')

jArray :: Parser String JValue
jArray = JArray <$>
    (char '['
     *> (jValue `separatedBy` char ',' `surroundedBy` spaces)
     <* char ']')

jObject :: Parser String JValue
jObject = JObject <$>
    (char '{' *> pair `separatedBy` char ',' `surroundedBy` spaces <* char '}')
    where
        pair = (\ ~(JString s) j -> (s,j))
            <$> (jString `surroundedBy` spaces)
            <* char ':'
            <*> jValue

jValue :: Parser String JValue
jValue = jValue' `surroundedBy` spaces
    where   
        jValue' = jNull
            <|> jBool
            <|> jString
            <|> jInt
            <|> jArray
            <|> jObject

doSum :: String -> Int
doSum s = case runParser jValue s of 
    Just (_, a) -> doSum' a
        
doSum' :: JValue -> Int
doSum' (JArray a) = sum (map doSum' a)
doSum' (JObject a) = sum (map (doSum' . snd) a)
doSum' (JInt a) = a
doSum' _ = 0

part1 = doSum day12Data

hasRed :: [(String, JValue)] -> Bool
hasRed a = isJust (find (\case 
    JString a -> (a =="red") 
    _ -> False) (map snd a))

doSum2 :: String -> Int
doSum2 s = case runParser jValue s of 
    Just (_, a) -> doSum2' a
        
doSum2' :: JValue -> Int
doSum2' (JArray a) = sum (map doSum2' a)
doSum2' (JObject a) = if hasRed a then 0 else sum (map (doSum2' . snd) a)
doSum2' (JInt a) = a
doSum2' _ = 0

part2 = doSum2 day12Data