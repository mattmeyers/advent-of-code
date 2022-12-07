module Main where

import Prelude hiding (lookup)
import Data.Text (splitOn, pack, unpack)
import Data.Sequence (Seq (Empty, (:|>)), fromList, update, lookup, viewr, foldMapWithIndex, foldlWithIndex)
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

parseInputMoves :: String -> [(Int, Int, Int)]
parseInputMoves s = map parseInputMove $ lines s

parseInputMove :: String -> (Int, Int, Int)
parseInputMove l = (n, src, dst)
    where
        ws = words l
        n = parseInt $ ws !! 1
        src = parseInt $ ws !! 3
        dst = parseInt $ ws !! 5

parseInt :: String -> Int
parseInt = read

run :: (State, [Move]) -> State
run (state, moves) = foldl applyMove state moves

run' :: (State, [Move]) -> State
run' (state, moves) = foldl applyMove' state moves

applyMove :: State -> Move -> State
applyMove s (0, src, dst) = s
applyMove s (n, src, dst) = applyMove (moveCrate s 1 src dst) (n-1, src, dst)

applyMove' :: State -> Move -> State
applyMove' s (n, src, dst) = moveCrate s n src dst

moveCrate :: State -> Int -> Int -> Int -> State
moveCrate s nCrates src dst = removeCrates (src-1) nCrates $ pushCrates s (dst-1) (take nCrates $ grabStack (src-1) s)

grabStack :: Int -> State -> [String]
grabStack n s = grabValue $ lookup n s

grabValue :: Maybe [String] -> [String]
grabValue Nothing = []
grabValue (Just v) = v

pushCrates :: State -> Int -> [String] -> State
pushCrates s n val = update n (val ++ grabStack n s) s

removeCrates :: Int -> Int -> State -> State
removeCrates stackNum nCrates s = update stackNum (drop nCrates $ grabStack stackNum s) s

init' :: [a] -> [a]
init' [] = []
init' xs = init xs

topCrates :: State -> String
topCrates = foldl (\acc v -> acc ++ head v ) ""

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print . topCrates . run $ parseInput contents
    print . topCrates . run' $ parseInput contents