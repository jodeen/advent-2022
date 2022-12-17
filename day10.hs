import DayTenData

doInstruction :: (Int, Int) -> String -> [(Int, Int)]
doInstruction (cycle, value) "noop" = [(cycle + 1, value)]
doInstruction (cycle, value) ('a':'d':'d':'x':' ':xs) = [(cycle + 1, value), (cycle + 2, value + (read xs))]


processInstructions :: [String] -> (Int, Int) -> [(Int, Int)]
processInstructions [] _ = [] 
processInstructions (instruction:xs) state = newStates ++ (processInstructions xs (last newStates))
    where
        newStates = doInstruction state instruction

calcSignal :: (Int, Int) -> Int
calcSignal (a,b) = a * b

isCriticalSignal :: (Int, Int) -> Bool
isCriticalSignal (cycle, _) = (mod cycle 40) == 20

part1 instructions = sum (map (\(a,b) -> a*b) (filter isCriticalSignal (processInstructions instructions (1,1))))

between i (a,b) = a <= i && i <= b

getPixel :: (Int, Int) -> Int -> Char
getPixel (cycle, value) column = if (column `between` (value-1, value+1)) then '#' else ' '

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n items = (take n items) : (chunk n (drop n items))

join :: String -> [String] -> String
join _ [line] = line
join sep (line:xs) = line ++ sep ++ (join sep xs)

renderScreen instructions = chunk 40 pixels
    where 
        signals = (1,1) : (processInstructions instructions (1,1))
        pixels = zipWith getPixel signals (cycle [0..39])

part2 instructions = putStr (join "\n" (renderScreen instructions))

sampleData = [
    "noop",
    "addx 3",
    "addx -5"
    ]

sampleData2 = [
    "addx 15",
    "addx -11",
    "addx 6",
    "addx -3",
    "addx 5",
    "addx -1",
    "addx -8",
    "addx 13",
    "addx 4",
    "noop",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx -35",
    "addx 1",
    "addx 24",
    "addx -19",
    "addx 1",
    "addx 16",
    "addx -11",
    "noop",
    "noop",
    "addx 21",
    "addx -15",
    "noop",
    "noop",
    "addx -3",
    "addx 9",
    "addx 1",
    "addx -3",
    "addx 8",
    "addx 1",
    "addx 5",
    "noop",
    "noop",
    "noop",
    "noop",
    "noop",
    "addx -36",
    "noop",
    "addx 1",
    "addx 7",
    "noop",
    "noop",
    "noop",
    "addx 2",
    "addx 6",
    "noop",
    "noop",
    "noop",
    "noop",
    "noop",
    "addx 1",
    "noop",
    "noop",
    "addx 7",
    "addx 1",
    "noop",
    "addx -13",
    "addx 13",
    "addx 7",
    "noop",
    "addx 1",
    "addx -33",
    "noop",
    "noop",
    "noop",
    "addx 2",
    "noop",
    "noop",
    "noop",
    "addx 8",
    "noop",
    "addx -1",
    "addx 2",
    "addx 1",
    "noop",
    "addx 17",
    "addx -9",
    "addx 1",
    "addx 1",
    "addx -3",
    "addx 11",
    "noop",
    "noop",
    "addx 1",
    "noop",
    "addx 1",
    "noop",
    "noop",
    "addx -13",
    "addx -19",
    "addx 1",
    "addx 3",
    "addx 26",
    "addx -30",
    "addx 12",
    "addx -1",
    "addx 3",
    "addx 1",
    "noop",
    "noop",
    "noop",
    "addx -9",
    "addx 18",
    "addx 1",
    "addx 2",
    "noop",
    "noop",
    "addx 9",
    "noop",
    "noop",
    "noop",
    "addx -1",
    "addx 2",
    "addx -37",
    "addx 1",
    "addx 3",
    "noop",
    "addx 15",
    "addx -21",
    "addx 22",
    "addx -6",
    "addx 1",
    "noop",
    "addx 2",
    "addx 1",
    "noop",
    "addx -10",
    "noop",
    "noop",
    "addx 20",
    "addx 1",
    "addx 2",
    "addx 2",
    "addx -6",
    "addx -11",
    "noop",
    "noop",
    "noop"]
