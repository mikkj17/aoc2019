import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

parse :: String -> [Int]
parse inp = map read $ splitOn "," inp

opcodeComputation :: Int -> Int -> Int -> Int
opcodeComputation 1 x y = x + y
opcodeComputation 2 x y = x * y

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i + 1) xs

run :: Int -> [Int] -> [Int]
run i xs =
  if opcode == 99
    then xs
    else run (i + 4) (replaceAtIndex output (opcodeComputation opcode x y) xs)
  where
    opcode = xs !! i
    x = xs !! (xs !! (i + 1))
    y = xs !! (xs !! (i + 2))
    output = xs !! (i + 3)

computeOutput :: [Int] -> Int
computeOutput program = head $ run 0 program

replace :: Int -> Int -> [Int] -> [Int]
replace noun verb xs = replaceAtIndex 2 verb $ replaceAtIndex 1 noun xs

first :: String -> Int
first inp = computeOutput $ replace 12 2 $ parse inp

search :: [Int] -> (Int, Int)
search program =
  fromJust $
    find
      (\(noun, verb) -> computeOutput (replace noun verb program) == 19690720)
      [(noun, verb) | noun <- [1 .. 99], verb <- [1 .. 99]]

second :: String -> Int
second inp =
  let (noun, verb) = search $ parse inp
   in 100 * noun + verb

main :: IO ()
main = do
  testInput <- readFile "test-input.txt"
  input <- readFile "input.txt"
  print $ first input
  print $ second input
