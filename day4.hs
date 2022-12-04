import DayFourData
sampleInput =[ 
    ((2, 4),(6, 8)),
    ((2, 3),(4, 5)),
    ((5, 7),(7, 9)),
    ((2, 8),(3, 7)),
    ((6, 6),(4, 6)),
    ((2, 6),(4, 8))] :: [ ((Int, Int), (Int, Int))]

fullyContained :: ((Int, Int), (Int, Int)) -> Bool
fullyContained ((a,b), (x,y)) = (a <= x && b >= y) || (x <= a && y >= b)

between a (x,y) = a >= x && a <= y

partiallyContained :: ((Int, Int), (Int, Int)) -> Bool
partiallyContained ((a,b), (x,y)) = (a `between` (x,y)) || (b `between` (x,y)) || x `between` (a,b) || y `between` (a,b)

part1 input = length (filter fullyContained input)

part2 input = length (filter partiallyContained input)