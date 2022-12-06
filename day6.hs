import Data.List (nub)
import DaySixData

isMarker :: Int -> String ->  Bool
isMarker size input = length (nub (take size input)) == size

isPacketMarker :: String ->  Bool
isPacketMarker = isMarker 4

window :: Int -> String -> Int -> [(String, Int)]
window size input idx 
    | (length input) < size = []
    | otherwise = (take size input, idx) : window size (tail input) (idx + 1)

isMessageMarker :: String -> Bool
isMessageMarker = isMarker 14

part1 input = idx + 4
    where
        (_, idx) = head (filter (isPacketMarker . fst) (window 4 input 0))

part2 input = idx + 14
    where
        (_, idx) = head (filter (isMessageMarker . fst) (window 14 input 0))        