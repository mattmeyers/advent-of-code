module Main where
import Data.Char (ord, isLower)

parseInput :: String -> [[String]]
parseInput content = map splitInHalf $ lines content

splitInHalf :: String -> [String]
splitInHalf s = [take len s, drop len s]
    where
        len = div (length s)  2

parseInput' :: String -> [[String]]
parseInput' content = chunk 3 (lines content)

chunk :: Int -> [String] -> [[String]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

findDuplicate :: [String] -> Char
findDuplicate xs = head $ foldl intersect (head xs) xs

intersect :: [Char] -> [Char] -> [Char]
intersect [] = const []
intersect xs = filter (`elem` xs)

scoreChar :: Char -> Int
scoreChar c
    | isLower c = ord c - 96 -- Shift [97, 122] -> [1,26]
    | otherwise = ord c - 38 -- Shift [65,90] -> [27,52]

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print . sum . map (scoreChar . findDuplicate) $ parseInput contents
    print . sum . map (scoreChar . findDuplicate) $ parseInput' contents