module Main where

import Data.Char (digitToInt)
import Data.List (transpose, iterate, foldl1)

parseInput :: String -> [[Int]]
parseInput s = map (map digitToInt) $ lines s

walkLine :: [Int] -> [Bool]
walkLine = walkLine' (-1)

walkLine' :: Int -> [Int] ->[Bool]
walkLine' _ [] = []
walkLine' prevMax (x:xs) = (x > prevMax) : walkLine' (max prevMax x) xs

generateVariations :: [[a]] -> [[[a]]]
generateVariations l = [
        l,
        rotateN 1 l,
        rotateN 2 l,
        rotateN 3 l
    ]

normalizeVariations :: [[[a]]] -> [[[a]]]
normalizeVariations = zipWith (curry normalize) [0..3]

normalize :: (Int, [[a]]) -> [[a]]
normalize (n, l) = rotateN' n l

rotateN :: Int -> [[a]] -> [[a]]
rotateN n a = iterate rotate a !! n

rotate :: [[a]] -> [[a]]
rotate = transpose . reverse

rotateN' :: Int -> [[a]] -> [[a]]
rotateN' n a = iterate rotate' a !! n

rotate' :: [[a]] -> [[a]]
rotate' = reverse . transpose

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

lookFromTree :: [Int] -> [Int]
lookFromTree [] = []
lookFromTree (x:xs) = lookFromTree' x xs : lookFromTree xs

lookFromTree' :: Int -> [Int] -> Int
lookFromTree' _ [] = 0
lookFromTree' x (y:ys)
    | y >= x = 1
    | otherwise = 1 + lookFromTree' x ys

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print . sum . map boolToInt $ foldl1 (zipWith (||)) $ map concat $ normalizeVariations $ map (map walkLine) (generateVariations $ parseInput contents)
    print . maximum . foldl1 (zipWith (*)) $ map concat $ normalizeVariations $ map (map lookFromTree) (generateVariations $ parseInput contents)