import DayFiveData

sampleDataStacks = ["NZ", "DCM", "P"]
sampleDataOps = [
    (1,2,1),
    (3,1,3),
    (2,2,1),
    (1,1,2)] :: [(Int, Int, Int)]


setValue :: [a] -> Int -> a -> [a]
setValue list idx newValue = start ++ [newValue] ++ (tail end)
    where 
        (start, end) = splitAt idx list


moveOp :: [String] -> (Int, Int, Int) -> [String]
moveOp stacks (amount, from, to) = setValue (setValue stacks (from-1) startStackNew) (to-1) endStackNew
    where
        startStackOld = stacks !! (from-1)
        endStackOld = stacks !! (to-1)
        (removed, startStackNew) = splitAt amount startStackOld
        endStackNew = (reverse removed) ++ endStackOld

part1 startStack moves = map head (foldl moveOp startStack moves)


moveOp2 :: [String] -> (Int, Int, Int) -> [String]
moveOp2 stacks (amount, from, to) = setValue (setValue stacks (from-1) startStackNew) (to-1) endStackNew
    where
        startStackOld = stacks !! (from-1)
        endStackOld = stacks !! (to-1)
        (removed, startStackNew) = splitAt amount startStackOld
        endStackNew = removed ++ endStackOld

part2 startStack moves = map head (foldl moveOp2 startStack moves)
