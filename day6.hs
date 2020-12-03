import Data.List
import DayData(day6)

type Instruction = (Char, (Int, Int), (Int, Int))
type State =[(Int, Int, Bool)]
type State2 =[(Int, Int, Int)]

initState :: Int -> State
initState bound = [(x,y,False) | x <- [0..bound-1], y<-[0..bound-1]]

initState2 :: Int -> State2
initState2 bound = [(x,y,0) | x <- [0..bound-1], y<-[0..bound-1]]


isIn (sx, sy) (ex,ey) (x,y) = sx <= x && ex >= x && sy <= y && ey >= y

rangeCheck :: (Int, Int) -> (Int, Int) -> (a -> a) -> (Int, Int, a) -> (Int, Int, a)
rangeCheck start end f (x,y,b) = (x,y, if (isIn start end (x,y)) then (f b) else b )

process :: State -> Instruction -> State
process state ('o', start, end) = map (rangeCheck start end (const True)) state
process state ('f', start, end) = map (rangeCheck start end (const False)) state
process state ('t', start, end) = map (rangeCheck start end (not)) state

part1 = length $ filter (\(x, y, t) -> t) finalState
    where 
        state = initState 1000
        finalState = foldl process state day6

rangeCheck2 :: (Int, Int) -> (Int, Int) -> (Int -> Int) -> (Int, Int, Int) -> (Int, Int, Int)
rangeCheck2 start end f (x,y,b) = (x,y, if (isIn start end (x,y)) then (f b) else b )

process2 :: State2 -> Instruction -> State2
process2 state ('o', start, end) = map (rangeCheck2 start end (+ 1)) state
process2 state ('f', start, end) = map (rangeCheck2 start end (\a -> max (a-1) 0)) state
process2 state ('t', start, end) = map (rangeCheck2 start end (+ 2)) state        

apply :: Char -> Int -> Int
apply 'o' a = 1 + a
apply 'f' a =  max (a-1) 0
apply 't' a = 2 + a

third (_,_,c) = c

brightnessStep :: (Int, Int) -> Int -> Instruction -> Int
brightnessStep point curr (inst, start, end) = if (isIn start end point) then (apply inst curr) else curr

brightness :: [Instruction] -> (Int, Int) ->  Int
brightness insts point  = foldl (brightnessStep point) 0 insts 


part2 = sum (map (brightness day6) items)
    where 
        items = [(x,y) | x <- [0..999], y<-[0..999]]
