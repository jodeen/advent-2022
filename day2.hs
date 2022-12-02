
import DayTwoData (day2Data)
data Play = Rock | Paper | Scissors deriving (Show)
data Outcome = Win | Lose | Draw deriving (Show)

sampleInput = [
    ('A', 'Y'), 
    ('B', 'X'),
    ('C', 'Z')]

roundScore :: (Play, Play) -> Int
roundScore (Rock, Paper) = 6
roundScore (Rock, Scissors) = 0
roundScore (Paper, Rock) = 0
roundScore (Paper, Scissors) = 6
roundScore (Scissors, Rock) = 6
roundScore (Scissors, Paper) = 0
roundScore (_, _) = 3

playerScore :: (Play, Play) -> Int
playerScore (_, Rock) = 1
playerScore (_, Paper) = 2
playerScore (_, Scissors) = 3

parseChar :: Char -> Play
parseChar 'A' = Rock
parseChar 'B' = Paper
parseChar 'C' = Scissors
parseChar 'X' = Rock
parseChar 'Y' = Paper
parseChar 'Z' = Scissors

parseInput :: [(Char, Char)] -> [(Play, Play)]
parseInput = map (\(a,b) -> (parseChar a, parseChar b))

calculateScores :: [(Play, Play)] -> [Int]
calculateScores = map (\play -> (playerScore play) + (roundScore play))

part1 input = sum (calculateScores plays)
    where 
        plays = parseInput input


parseOutcome :: Char -> Outcome
parseOutcome 'X' = Lose
parseOutcome 'Y' = Draw
parseOutcome 'Z' = Win


parseInput2 :: [(Char, Char)] -> [(Play, Outcome)]
parseInput2 = map (\(a,b) -> (parseChar a, parseOutcome b))

outcomeToPlay :: (Play, Outcome) -> (Play, Play)
outcomeToPlay (Rock, Win) = (Rock, Paper)
outcomeToPlay (Rock, Lose) = (Rock, Scissors)
outcomeToPlay (Paper, Win) = (Paper, Scissors)
outcomeToPlay (Paper, Lose) = (Paper, Rock)
outcomeToPlay (Scissors, Win) = (Scissors, Rock)
outcomeToPlay (Scissors, Lose) = (Scissors, Paper)
outcomeToPlay (x, Draw) = (x, x)

part2 input = sum (calculateScores plays)
    where 
        outcomes = parseInput2 input
        plays = map outcomeToPlay outcomes
