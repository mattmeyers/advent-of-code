module Main where

import qualified Data.Text as T
import qualified Data.List as L

buildLists :: String -> [[Int]]
buildLists contents = split $ T.splitOn  (T.pack "\n\n") (T.pack contents)

split :: [T.Text] -> [[Int]]
split = convert . map (lines . T.unpack)

convert :: [[String]] -> [[Int]]
convert = map (map read)

sumLists :: [[Int]] -> [Int]
sumLists = map sum

sumTop3 :: [Int] -> Int
sumTop3 l = sum . L.take 3 $ L.reverse $ L.sort l

part1 :: String -> Int
part1 contents = maximum . sumLists $ buildLists contents

part2 :: String -> Int
part2 contents =  sumTop3 . sumLists $ buildLists contents

main = do
    contents <- readFile "./input.txt"
    print $ "PART 1: " ++ show (part1 contents)
    print $ "PART 2: " ++ show (part2 contents)