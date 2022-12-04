module Main where

import Data.Text (Text, pack, unpack, splitOn)

parseInput :: String -> [[[Int]]]
parseInput contents = map parseLine $ lines contents

parseLine :: String -> [[Int]]
parseLine = map generateRange . splitLine. pack

splitLine :: Text -> [Text]
splitLine = splitOn (pack ",")

generateRange :: Text -> [Int]
generateRange t = [(parseInt m)..(parseInt n)]
    where
        [m, n] = splitOn (pack "-") t

parseInt :: Text -> Int
parseInt t = read $ unpack t

fullyOverlaps :: [[Int]] -> Bool
fullyOverlaps ls
    | overlapLength > 0 = isFullOverlap overlapLength ls
    | otherwise = False
    where
        overlapLength = length $ intersect (head ls) (last ls)

isFullOverlap :: Int -> [[Int]] -> Bool
isFullOverlap ol = any (\l -> ol == length l)

overlaps :: [[Int]] -> Bool
overlaps ls = overlapLength > 0
    where
        overlapLength = length $ intersect (head ls) (last ls)


intersect :: [Int] -> [Int] -> [Int]
intersect [] = const []
intersect xs = filter (`elem` xs)

solve :: [[[Int]]] -> Int
solve ls = length $ filter fullyOverlaps ls

solve' :: [[[Int]]] -> Int
solve' ls = length $ filter overlaps ls

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print (solve (parseInput contents))
    print (solve' (parseInput contents))