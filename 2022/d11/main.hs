module Main where

import Data.List (sort, reverse)
import Data.Map.Strict (Map, adjust, fromList, (!))
import Data.Text (Text, pack, splitOn, unpack)

data Monkey = Monkey
  { items :: [Int],
    op :: Int -> Int,
    test :: Int,
    ifTrue :: Int,
    ifFalse :: Int,
    inspections :: Int
  }

--   deriving (Show)

type MonkeyMap = Map Int Monkey

parseInput :: String -> Map Int Monkey
parseInput s = fromList $ zip [0 .. 7] $ map (parseMonkey . splitOn (pack "\n")) $ splitOn (pack "\n\n") (pack s)

parseMonkey :: [Text] -> Monkey
parseMonkey ls =
  Monkey
    { items = is,
      op = parseOp $ ls !! 2,
      test = lastInt $ ls !! 3,
      ifTrue = lastInt $ ls !! 4,
      ifFalse = lastInt $ ls !! 5,
      inspections = length is
    }
    where
        is = parseItems $ ls !! 1

parseItems :: Text -> [Int]
parseItems l = map parseInt $ splitOn (pack ", ") $ last $ splitOn (pack ": ") l

lastInt :: Text -> Int
lastInt s = parseInt $ last $ splitOn (pack " ") s

parseOp :: Text -> (Int -> Int)
parseOp t = parseOp' . map unpack $ splitOn (pack " ") $ last $ splitOn (pack "= ") t

parseOp' :: [String] -> (Int -> Int)
parseOp' ["old", "*", "old"] = \n -> n * n
parseOp' ["old", "*", x] = \n -> n * read x
parseOp' ["old", "+", x] = \n -> n + read x
parseOp' _ = error "cannot parse operation"

parseInt :: Text -> Int
parseInt = read . unpack

inspectItems :: Int -> MonkeyMap -> MonkeyMap
inspectItems monkeyNum mm
  | null (items (mm ! monkeyNum)) = mm
  | otherwise = inspectItems monkeyNum $ inspectItem mm monkeyNum

inspectItem :: MonkeyMap -> Int -> MonkeyMap
inspectItem mm monkeyNum = catchItem target worry $ throwItem mm monkeyNum
  where
    monkey = mm ! monkeyNum
    worry = updateWorryLevel monkey
    target = chooseTarget monkey worry

updateWorryLevel :: Monkey -> Int
updateWorryLevel m = div (op m worry) 3
  where
    worry = head $ items m

chooseTarget :: Monkey -> Int -> Int
chooseTarget m worry =
  if mod worry (test m) == 0
    then ifTrue m
    else ifFalse m

throwItem :: MonkeyMap -> Int -> MonkeyMap
throwItem mm monkeyNum =
  adjust
    (\m -> m {items = tail $ items m})
    monkeyNum
    mm

catchItem :: Int -> Int -> MonkeyMap -> MonkeyMap
catchItem monkeyNum worry =
  adjust
    (\m -> m {items = items m ++ [worry], inspections = 1 + inspections m})
    monkeyNum

runOnce :: MonkeyMap -> MonkeyMap
runOnce mm = foldl (flip inspectItems) mm [0 .. 7]

run :: MonkeyMap -> MonkeyMap
run mm = foldl (\acc i -> runOnce acc) mm [0 .. 19]

listItems :: MonkeyMap -> [[Int]]
listItems mm = foldl (\acc i -> acc ++ [items $ mm ! i]) [] [0 .. 7]

listInspections :: MonkeyMap -> [Int]
listInspections mm = foldl (\acc i -> acc ++ [inspections (mm ! i) - length (items $ mm ! i)]) [] [0 .. 3]

calculateMonkeyBusiness :: MonkeyMap -> Int
calculateMonkeyBusiness mm = product $ take 2 $ reverse . sort $ listInspections mm

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let mm = run $ parseInput contents
  print $ calculateMonkeyBusiness mm