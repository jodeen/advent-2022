import Data.List (mapAccumR, transpose, mapAccumL, zipWith4, maximum)
import Data.Char (digitToInt)
import DayEightData


parseInput :: [String] -> [[Int]]
parseInput = map (map digitToInt)

findVisibleDirection :: [Int] -> [Bool]
findVisibleDirection trees = snd (mapAccumR (\tallest curr -> (max tallest curr, curr > tallest)) (-1) trees)
    
-- we only look at visibility in one direction (and then we 
-- transform the grid to get everything looking from that direction)
findVisible :: [Int] -> [Bool]
findVisible trees =  zipWith (||) left right 
    where 
        left = findVisibleDirection trees
        right = reverse (findVisibleDirection (reverse trees))

combine :: [[Bool]] -> [[Bool]] -> [[Bool]]
combine = zipWith (zipWith (||)) 

toVisible :: [[Int]] -> [[Bool]]
toVisible input = combine leftRight upDown
    where
        leftRight = map findVisible input
        upDown = transpose (map findVisible (transpose input))

part1 input = length (filter id (concat (toVisible input)))

updateCurrentViews :: [Int] -> Int -> [Int]
updateCurrentViews views height = (map (const 1) pre) ++ (map (+ 1) post)
    where
        (pre, post) = splitAt (height + 1) views

findView :: [Int] -> [Int]
findView trees = views
    where
        (_,views) = mapAccumL (\acc curr -> (updateCurrentViews acc curr, acc !! curr)) (replicate 11 0) trees

findViewGrid :: [[Int]] -> [[Int]]
findViewGrid = map findView

combineViews :: [[Int]] -> [Int]
combineViews grid = zipWith4 (\a b c d -> a * b * c * d) (concat left) (concat right) (concat up) (concat down)
    where 
        left = findViewGrid grid
        right = map reverse (findViewGrid (map reverse grid))
        up = transpose (findViewGrid (transpose grid))
        -- we have to make sure to undo the transforms in the same order
        down = transpose (map reverse (findViewGrid (map reverse (transpose grid))))

part2 input = maximum (combineViews input)

sampleInput = parseInput [
    "30373", 
    "25512",
    "65332",
    "33549",
    "35390"]