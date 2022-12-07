module Main where

import Prelude hiding (lookup)
import Data.Text (splitOn, pack, unpack)
import Data.Sequence (Seq, fromList, update, lookup)
import Data.List (transpose)

type State = Seq [String]
type Move = (Int, Int, Int)

parseInput :: String -> (State, [Move])
parseInput s = (parseInputState inputState, parseInputMoves moves)
    where
        parts = splitOn (pack "\n\n") (pack s)
        inputState = unpack $ head parts
        moves = unpack $ last parts

parseInputState :: String -> State
parseInputState s = fromList . fmap (reverse . parseLine) . transpose . tail . reverse . map splitLine $ lines s

splitLine :: String -> [String]
splitLine s = map (take 3) $ chunk 4 (s ++ " ")

parseLine :: [String] -> [String]
parseLine [] = []
parseLine ("   ":_) = []
parseLine (x:xs) = [head (tail x)]:parseLine xs

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

parseInputMoves :: String -> [Move]
parseInputMoves s = map parseInputMove $ lines s

parseInputMove :: String -> Move
parseInputMove l = (n, src-1, dst-1)
    where
        ws = words l
        n = parseInt $ ws !! 1
        src = parseInt $ ws !! 3
        dst = parseInt $ ws !! 5

parseInt :: String -> Int
parseInt = read

solve1 :: (State, [Move]) -> String
solve1 (state, moves) = topCrates $ foldl applyMove state moves

solve2 :: (State, [Move]) -> String
solve2 (state, moves) = topCrates $ foldl applyMove' state moves

applyMove :: State -> Move -> State
applyMove s (0, src, dst) = s
applyMove s (n, src, dst) = applyMove (moveCrate s 1 src dst) (n-1, src, dst)

applyMove' :: State -> Move -> State
applyMove' s (n, src, dst) = moveCrate s n src dst

moveCrate :: State -> Int -> Int -> Int -> State
moveCrate s nCrates src dst = removeCrates src nCrates $ pushCrates s dst (take nCrates $ grabStack src s)

grabStack :: Int -> State -> [String]
grabStack n s = grabValue $ lookup n s

grabValue :: Maybe [String] -> [String]
grabValue Nothing = []
grabValue (Just v) = v

pushCrates :: State -> Int -> [String] -> State
pushCrates s n val = update n (val ++ grabStack n s) s

removeCrates :: Int -> Int -> State -> State
removeCrates stackNum nCrates s = update stackNum (drop nCrates $ grabStack stackNum s) s

topCrates :: State -> String
topCrates = foldl (\acc v -> acc ++ head v ) ""

run :: ((State, [Move]) -> String) -> String -> IO ()
run f filename = do
    contents <- readFile filename
    print . f $ parseInput contents

main :: IO ()
main = do
    run solve1 "sample.txt"
    run solve1 "input.txt"
    run solve2 "sample.txt"
    run solve2 "input.txt"