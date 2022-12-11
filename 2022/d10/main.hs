module Main where

import Data.Sequence (fromList, index)

data Instruction = Addx Int | Nop deriving (Show, Eq)

parseInput :: String -> [Instruction]
parseInput = map (parseInstruction . words) . lines

parseInstruction :: [String] -> Instruction
parseInstruction ["addx", v] = Addx $ read v
parseInstruction ["noop"] = Nop
parseInstruction _ = error "unknown instruction"

run :: [Int] -> [Instruction] -> [Int]
run = foldl (\ acc i -> acc ++ parseLine acc i)

parseLine :: [Int] -> Instruction -> [Int]
parseLine s (Addx n) = [last s, n + last s]
parseLine s Nop =  [last s]

calcResult :: [Int] -> Int
calcResult ls = foldl (\acc i -> acc + f i) 0 [20,60..220]
    where
        s = fromList ls
        f = \n -> n * index s (n-1)

run' :: [Instruction] -> [String]
run' is = map (drawLine 0) (chunk 40 $ run [1] is)

drawLine :: Int -> [Int] -> [Char]
drawLine _ [] = []
drawLine currentCycle (spritePos:ps)
    | abs (currentCycle - spritePos) <= 1 = '#' : drawLine (currentCycle+1) ps
    | otherwise = '.' : drawLine (currentCycle+1) ps

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print . calcResult $ run [1] $ parseInput contents
    putStr . unlines . run' $ parseInput contents