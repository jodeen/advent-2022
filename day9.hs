import Data.List (nub)
import DayNineData


data Dir = U | Down | Left | Right deriving (Show)

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail (hx, hy) (tx, ty) 
    | hy-2 == ty && hx == tx = (tx, ty+1)
    | hy+2 == ty && hx == tx = (tx, ty-1)
    | hy == ty && hx-2 == tx = (tx+1, ty)
    | hy == ty && hx+2 == tx = (tx-1, ty)
    | (hy-2 == ty && hx-1 == tx) ||
        (hy-1 == ty && hx-2 == tx) = (tx+1, ty+1)
    | (hy-2 == ty && hx+1 == tx) ||
        (hy-1 == ty && hx+2 == tx) = (tx-1, ty+1)        
    | (hy+2 == ty && hx+1 == tx) ||
        (hy+1 == ty && hx+2 == tx) = (tx-1, ty-1)        
    | (hy+2 == ty && hx-1 == tx) ||
        (hy+1 == ty && hx-2 == tx) = (tx+1, ty-1)     
    | otherwise = (tx, ty)


moveHead :: String -> (Int, Int) -> (Int, Int)
moveHead "U" (x,y) = (x, y+1)
moveHead "D" (x,y) = (x, y-1)
moveHead "L" (x,y) = (x-1, y)
moveHead "R" (x,y) = (x+1, y)

doMove :: (String, Int) -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
doMove (dir, amount) (hStart, tStart) = (moveHead dir hStart, moveTail hStart tStart)


doMoveRope :: (String, Int) -> [(Int, Int)] -> [(Int, Int)]
doMoveRope (dir, amount) (h:xs) = scanl moveTail (moveHead dir h) xs 

processMoves :: [(String, Int)] -> ((Int, Int), (Int, Int)) -> [((Int, Int), (Int, Int))]
processMoves [] state = [state]
processMoves ((_, 0) : xs) state = processMoves xs state
processMoves ((dir, amount) : xs) (h, t) =  newState : (processMoves ((dir, amount-1) : xs) newState)
    where
        newHead = moveHead dir h
        newState = (newHead, moveTail newHead t)

part1 input = length (nub (map snd (processMoves input ((0,0), (0,0)))))

processMovesRope :: [(String, Int)] -> [(Int, Int)] -> [[(Int, Int)]]
processMovesRope [] state = []
processMovesRope ((_, 0) : xs) state = processMovesRope xs state
processMovesRope ((dir, amount) : xs) rope =  newRope : (processMovesRope ((dir, amount-1) : xs) newRope)
    where
        newRope = doMoveRope (dir, amount) rope

part2 input = last (processMovesRope input (replicate 10 (0,0)))

sampleData = [
    ("R", 4),
    ("U", 4),
    ("L", 3),
    ("D", 1),
    ("R", 4),
    ("D", 1),
    ("L", 5),
    ("R", 2)] :: [(String, Int)]

sampleData2 =  [
    ("R", 5),
    ("U", 8),
    ("L", 8),
    ("D", 3),
    ("R", 17),
    ("D", 10),
    ("L", 25),
    ("U", 20)] :: [(String, Int)]

