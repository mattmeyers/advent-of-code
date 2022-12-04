module Main where

import Data.Text (Text, pack, unpack, splitOn)

parseInput :: String -> [[[Int]]]
parseInput contents = map parseLine $ lines contents

parseLine :: String -> [[Int]]
parseLine = map splitRange . splitLine. pack

splitLine :: Text -> [Text]
splitLine = splitOn (pack ",")

splitRange :: Text -> [Int]
splitRange t = generateRange (parseInt m) (parseInt n)
    where
        [m, n] = splitOn (pack "-") t

parseInt :: Text -> Int
parseInt t = read $ unpack t

generateRange :: Int -> Int -> [Int]
generateRange min max = [min..max]

overlaps :: [[Int]] -> Bool
overlaps ls
    | overlapLength > 0 = isFullOverlap overlapLength ls
    | otherwise = False
    where
        overlapLength = length $ intersect (head ls) (last ls)

overlaps' :: [[Int]] -> Bool
overlaps' ls = overlapLength > 0
    where
        overlapLength = length $ intersect (head ls) (last ls)

isFullOverlap :: Int -> [[Int]] -> Bool
isFullOverlap ol = any (\l -> ol == length l)

intersect :: [Int] -> [Int] -> [Int]
intersect [] = const []
intersect xs = filter (`elem` xs)

solve :: [[[Int]]] -> Int
solve ls = length $ filter overlaps ls

solve' :: [[[Int]]] -> Int
solve' ls = length $ filter overlaps' ls

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print (solve (parseInput contents))
    print (solve' (parseInput contents))