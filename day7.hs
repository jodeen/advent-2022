{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
import Data.List (isPrefixOf, sort)
import Data.Maybe (fromJust)
import DaySevenData

data File = File {fileSize :: Int, fileName :: String} deriving (Show)
data Directory = Directory {dirName :: String, files :: [File], dirs :: [Directory]} deriving Show

parseFile :: String -> File 
parseFile line = File { fileName = name, fileSize = read size}
    where 
        [size, name] = words line

parseDir :: String -> Directory 
parseDir line = Directory {dirName = name, files = [], dirs = []}
    where
        [_, _, name] = words line

doScan :: Maybe Directory -> [String] -> (Directory, [String])
doScan Nothing (line:xs) = doScan (Just (parseDir line)) xs
doScan (Just context) (line:xs) 
    | "$ cd .." == line = (context, xs)
    | "$ cd " `isPrefixOf` line = doScan (Just (context {dirs = newDir : (dirs context)})) remaining
    | "$ ls" == line = doScan (Just context) xs
    | "dir " `isPrefixOf` line = doScan (Just context) xs
    | otherwise = doScan (Just (context {files = (parseFile line) : (files context)})) xs
    where
        (newDir, remaining) = doScan (Just (parseDir line)) xs
doScan (Just context) [] = (context, [])


size :: Directory -> Int
size dir = sum (map fileSize (files dir)) + (sum (map size (dirs dir)))

flattenDir :: Directory -> [Directory]
flattenDir dir = if (null (dirs dir)) then 
        [dir]
    else 
       dir : (concatMap flattenDir (dirs dir))

rootDir = Directory {dirName = "/", files=[], dirs = []}

part1 input = sum (filter (<= 100000) (map size (flattenDir dir)))
    where
        (dir, _) = doScan Nothing input

part2 input = filter (> needToFree) (sort (map size flatDirs))
    where
        (dir, _) = doScan Nothing input
        flatDirs = flattenDir dir
        unused = 70000000 - size dir 
        needToFree = 30000000 - unused


sampleData = [
    "$ cd /",
    "$ ls",
    "dir a",
    "14848514 b.txt",
    "8504156 c.dat",
    "dir d",
    "$ cd a",
    "$ ls",
    "dir e",
    "29116 f",
    "2557 g",
    "62596 h.lst",
    "$ cd e",
    "$ ls",
    "584 i",
    "$ cd ..",
    "$ cd ..",
    "$ cd d",
    "$ ls",
    "4060174 j",
    "8033020 d.log",
    "5626152 d.ext",
    "7214296 k"]