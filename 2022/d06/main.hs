module Main where

solve1 :: String -> Int
solve1 = solve' 4 4

solve2 :: String -> Int
solve2 = solve' 14 14

solve' :: Int -> Int -> String -> Int
solve' position windowSize str
    | uniqueSet $ take windowSize str = position
    | otherwise = solve' (position+1) windowSize (tail str)

uniqueSet :: (Eq a) => [a] -> Bool
uniqueSet [] = True
uniqueSet (x:xs) = x `notElem` xs && uniqueSet xs

run :: (String -> Int) -> String -> IO ()
run f filename = do
    content <- readFile filename
    print $ f content

main :: IO ()
main = do
    run solve1 "sample1.txt"
    run solve1 "sample2.txt"
    run solve1 "sample3.txt"
    run solve1 "sample4.txt"
    run solve1 "sample5.txt"
    run solve1 "input.txt"

    run solve2 "sample1.txt"
    run solve2 "sample2.txt"
    run solve2 "sample3.txt"
    run solve2 "sample4.txt"
    run solve2 "sample5.txt"
    run solve2 "input.txt"