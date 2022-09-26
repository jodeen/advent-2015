import Day16Data
  ( Info
      ( Info,
        akitas,
        cars,
        cats,
        children,
        goldfish,
        perfumes,
        pomeranians,
        samoyeds,
        sue,
        trees,
        vizslas
      ),
      emptyInfo, 
    day16Data,
  )
import Data.Maybe

isMatch :: Info -> Bool
isMatch 
  Info { 
      children = children,
      cats = cats,
      samoyeds = samoyeds,
      pomeranians = pomeranians,
      akitas = akitas,
      vizslas = vizslas,
      goldfish = goldfish,
      trees = trees,
      cars = cars,
      perfumes = perfumes
    } = (3 == fromMaybe 3 children) && (7 == fromMaybe 7 cats) && (2 == fromMaybe  2 samoyeds) &&
    (3 == fromMaybe 3 pomeranians) && (0 == fromMaybe 0 akitas) && (0 == fromMaybe  0 vizslas) &&
    (5 == fromMaybe 5 goldfish) && (3 == fromMaybe 3 trees) && (2 == fromMaybe  2 cars) &&
    (1 == fromMaybe  1 perfumes)

isMatch2 :: Info -> Bool
isMatch2 
  Info { 
      children = children,
      cats = cats,
      samoyeds = samoyeds,
      pomeranians = pomeranians,
      akitas = akitas,
      vizslas = vizslas,
      goldfish = goldfish,
      trees = trees,
      cars = cars,
      perfumes = perfumes
    } = (3 == fromMaybe 3 children) && (7 < fromMaybe 8 cats) && (2 == fromMaybe  2 samoyeds) &&
    (3 > fromMaybe 2 pomeranians) && (0 == fromMaybe 0 akitas) && (0 == fromMaybe  0 vizslas) &&
    (5 > fromMaybe 4 goldfish) && (3 < fromMaybe 4 trees) && (2 == fromMaybe  2 cars) &&
    (1 == fromMaybe  1 perfumes)


part1 = head (filter isMatch day16Data)
part2 = head (filter isMatch2 day16Data)
