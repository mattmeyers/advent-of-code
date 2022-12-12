module Main where

import Data.List (reverse, sort)
import Data.Map.Strict (Map, adjust, elems, fromList, size, (!))
import Data.Text (Text, pack, splitOn, unpack)

data Monkey = Monkey
  { items :: [Int],
    op :: Int -> Int,
    test :: Int,
    ifTrue :: Int,
    ifFalse :: Int,
    inspections :: Int
  }

type MonkeyMap = Map Int Monkey

type UpdateWorryFn = Monkey -> Int

parseInput :: String -> MonkeyMap
parseInput s = fromList $ zip [0 .. (length monkeys - 1)] monkeys
  where
    monkeys = map (parseMonkey . splitOn (pack "\n")) $ splitOn (pack "\n\n") (pack s)

parseMonkey :: [Text] -> Monkey
parseMonkey ls =
  Monkey
    { items = is,
      op = parseOp test $ ls !! 2,
      test = test,
      ifTrue = lastInt $ ls !! 4,
      ifFalse = lastInt $ ls !! 5,
      inspections = length is
    }
  where
    is = parseItems $ ls !! 1
    test = lastInt $ ls !! 3

parseItems :: Text -> [Int]
parseItems l = map parseInt $ splitOn (pack ", ") $ last $ splitOn (pack ": ") l

lastInt :: Text -> Int
lastInt s = parseInt $ last $ splitOn (pack " ") s

parseOp :: Int -> Text -> (Int -> Int)
parseOp n t = parseOp' n $ map unpack $ splitOn (pack " ") $ last $ splitOn (pack "= ") t

parseOp' :: Int -> [String] -> (Int -> Int)
parseOp' _ ["old", "*", "old"] = \n -> n * n
parseOp' _ ["old", "*", x] = \n -> n * read x
parseOp' _ ["old", "+", x] = \n -> n + read x
parseOp' _ _ = error "cannot parse operation"

parseInt :: Text -> Int
parseInt = read . unpack

inspectItems :: UpdateWorryFn -> Int -> MonkeyMap -> MonkeyMap
inspectItems f monkeyNum mm
  | null (items (mm ! monkeyNum)) = mm
  | otherwise = inspectItems f monkeyNum $ inspectItem mm monkeyNum f

inspectItem :: MonkeyMap -> Int -> UpdateWorryFn -> MonkeyMap
inspectItem mm monkeyNum f = catchItem target worry $ throwItem mm monkeyNum
  where
    monkey = mm ! monkeyNum
    worry = f monkey
    target = chooseTarget monkey worry

updateWorryLevel1 :: UpdateWorryFn
updateWorryLevel1 m = div (op m worry) 3
  where
    worry = head $ items m

updateWorryLevel2 :: MonkeyMap -> UpdateWorryFn
updateWorryLevel2 mm m = mod (op m worry) $ sumTestValues mm
  where
    worry = head $ items m

sumTestValues :: MonkeyMap -> Int
sumTestValues mm = foldl (\acc m -> acc * test m) 1 $ elems mm

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

runOnce :: MonkeyMap -> UpdateWorryFn -> MonkeyMap
runOnce mm f = foldl (flip (inspectItems f)) mm [0 .. (size mm -1)]

run :: MonkeyMap -> UpdateWorryFn -> Int -> MonkeyMap
run mm f n = foldl (\acc i -> runOnce acc f) mm [1 .. n]

listInspections :: MonkeyMap -> [Int]
listInspections mm = foldl (\acc i -> acc ++ [inspections (mm ! i) - length (items $ mm ! i)]) [] [0 .. (size mm - 1)]

calculateMonkeyBusiness :: MonkeyMap -> Int
calculateMonkeyBusiness mm = product $ take 2 $ reverse . sort $ listInspections mm

solve1 :: String -> Int
solve1 s = calculateMonkeyBusiness $ run (parseInput s) updateWorryLevel1 20

solve2 :: String -> Int
solve2 s = calculateMonkeyBusiness $ run mm (updateWorryLevel2 mm) 10000
  where
    mm = parseInput s

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ solve1 contents
  print $ solve2 contents