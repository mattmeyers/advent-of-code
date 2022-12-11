module Main where

import Data.Sequence (fromList, index)

data Instruction = Addx Int | Nop deriving (Show, Eq)

parseInput :: String -> [Instruction]
parseInput = map (parseInstruction . words) . lines

parseInstruction :: [String] -> Instruction
parseInstruction ["addx", v] = Addx $ read v
parseInstruction ["noop"] = Nop
parseInstruction _ = error "unknown instruction"

calculateRegister :: [Instruction] -> [Int]
calculateRegister = foldl (\ acc i -> acc ++ runInstruction acc i) [1]

runInstruction :: [Int] -> Instruction -> [Int]
runInstruction s (Addx n) = [last s, n + last s]
runInstruction s Nop =  [last s]

calcTotalSignalStrength :: [Int] -> Int
calcTotalSignalStrength ls = foldl (\acc i -> acc + calcSignalStrength i) 0 [20,60..220]
    where
        s = fromList ls
        calcSignalStrength = \n -> n * index s (n-1)

drawLines :: [Instruction] -> [String]
drawLines is = map drawLine $ chunk 40 $ calculateRegister is

drawLine :: [Int] -> String
drawLine = drawPixel 0

drawPixel :: Int -> [Int] -> [Char]
drawPixel _ [] = []
drawPixel currentCycle (spritePos:ps)
    | abs (currentCycle - spritePos) <= 1 = '#' : drawPixel (currentCycle+1) ps
    | otherwise = '.' : drawPixel (currentCycle+1) ps

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

solve1 :: String -> Int
solve1 s = calcTotalSignalStrength $ calculateRegister $ parseInput s

solve2 :: String -> String
solve2 s = unlines . drawLines $ parseInput s

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print $ solve1 contents
    putStr $ solve2 contents