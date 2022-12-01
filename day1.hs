import DayOneData
import Data.List ( maximumBy, sort )
import Data.Function (on)

splitOnEmpty :: [String] -> [[String]]
splitOnEmpty [] = []
splitOnEmpty items = sublist : (splitOnEmpty (drop 1 rest))
    where
        (sublist, rest) = break (== "") items



parseInput :: [String] -> [[Int]]
parseInput input = map (\strings -> map (\s -> read s :: Int) strings) splitted
    where
        splitted = splitOnEmpty input

totalCalories :: [[Int]] -> [Int]
totalCalories = map sum

part1 input = maximum calories
    where
        elves = parseInput input
        calories = totalCalories elves

part2 input = sum (take 3 (reverse (sort calories)))
    where
        elves = parseInput input
        calories = totalCalories elves

sampleData1 = ["1000",
                "2000",
                "3000",
                "",
                "4000",
                "",
                "5000",
                "6000",
                "",
                "7000",
                "8000",
                "9000",
                "",
                "10000"]