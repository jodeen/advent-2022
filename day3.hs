import Data.List (splitAt)
import qualified Data.Set as Set
import Data.Char (ord)
import DayThreeData

compartmentalize :: String -> (String, String)
compartmentalize input = splitAt (length input `div` 2) input

commonItems :: (String, String) -> Set.Set Char
commonItems (a, b) = Set.intersection (Set.fromList a) (Set.fromList b)

priority :: Char -> Int
priority c = if (ord c) >= 97 then (ord c) - 96 else ord c - 38


part1 :: [String] -> Int
part1 input = sum (map priority allCommon)
    where
        allCommon = concatMap (\m -> Set.toList (commonItems (compartmentalize m))) input

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n items = (take n items) : (chunk n (drop n items))

badge :: [[Char]] -> Char
badge groupBags = head (Set.toList (foldr1 Set.intersection (map Set.fromList groupBags)))
    
part2 input = sum (map priority badges)
    where
        chunks = chunk 3 input
        badges = map badge chunks

sampleInput = [
    "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw"]
