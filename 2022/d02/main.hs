module Main where

import Data.List (reverse)

data GameResult = Win | Draw | Lose deriving (Eq,Ord,Enum,Show)
data Move = Rock | Paper | Scissors deriving (Eq,Ord,Enum,Show)

parseInput :: String -> [[Move]]
parseInput content = map  (reverse . map parseMove . words) $ lines content

parseInput' :: String -> [(Move, GameResult)]
parseInput' content = map (parseMoveAndResult . words) (lines content)

parseMoveAndResult :: [String] -> (Move, GameResult)
parseMoveAndResult [a, b] = (parseMove a, parseResult  b)
parseMoveAndResult _ = (Rock, Win)

parseMove :: String -> Move
parseMove m
    | m == "X" || m == "A" = Rock
    | m == "Y" || m == "B" = Paper
    | otherwise = Scissors

parseResult :: String -> GameResult
parseResult "X" = Lose
parseResult "Y" = Draw
parseResult _ = Win

chooseMove :: (Move, GameResult) -> [Move]
chooseMove (a, Draw) = [a, a]
chooseMove (a, Win) = [a, chooseWinningMove a]
chooseMove (a, Lose) = [a, chooseLosingMove a]

chooseWinningMove :: Move -> Move
chooseWinningMove Rock = Paper
chooseWinningMove Paper = Scissors
chooseWinningMove Scissors = Rock

chooseLosingMove :: Move -> Move
chooseLosingMove Rock = Scissors
chooseLosingMove Paper = Rock
chooseLosingMove Scissors = Paper

playGame :: [Move] -> GameResult
playGame [Rock,Scissors] = Win
playGame [Paper,Rock] = Win
playGame [Scissors,Paper] = Win
playGame a
    | all (== head a) a = Draw
    | otherwise = Lose


scoreMatch :: Move -> GameResult -> Int
scoreMatch m r = scoreMove m + scoreResult r

scoreMove :: Move -> Int
scoreMove Rock = 1
scoreMove Paper = 2
scoreMove Scissors = 3

scoreResult :: GameResult -> Int
scoreResult Win = 6
scoreResult Draw = 3
scoreResult Lose = 0


main :: IO ()
main = do
    contents <- readFile "./input.txt"
    print . sum $ map (\l -> scoreMatch (head l) (playGame l)) $ parseInput contents
    print . sum $ map ((\l -> scoreMatch (head l) (playGame l)) . reverse . chooseMove) $ parseInput' contents