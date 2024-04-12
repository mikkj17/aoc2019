import Data.Char (digitToInt)
import Data.List.Split (splitOn)

parse :: String -> [Int]
parse inp = map read $ splitOn "," inp

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i + 1) xs

zeroPad :: String -> String
zeroPad s = replicate (5 - length s) '0' ++ s

toParams :: Int -> (Int, [Int])
toParams x = (opcode, modes)
  where
    digits = zeroPad $ show x
    opcode = read $ drop 3 digits :: Int
    modes = reverse . map digitToInt $ take 3 digits

getByMode :: Int -> Int -> [Int] -> Int
getByMode ptr mode mem =
  if mode == 1
    then mem !! ptr
    else mem !! (mem !! ptr)

run :: Int -> Int -> [Int] -> [Int] -> [Int]
run inputId ptr mem output =
  case opcode of
    99 -> output
    1 -> run inputId (ptr + 4) (replaceAtIndex z (x + y) mem) output
    2 -> run inputId (ptr + 4) (replaceAtIndex z (x * y) mem) output
    3 -> run inputId (ptr + 2) (replaceAtIndex (mem !! (ptr + 1)) inputId mem) output
    4 -> run inputId (ptr + 2) mem (x : output)
    5 -> run inputId (if x /= 0 then y else ptr + 3) mem output
    6 -> run inputId (if x == 0 then y else ptr + 3) mem output
    7 -> run inputId (ptr + 4) (replaceAtIndex z (if x < y then 1 else 0) mem) output
    8 -> run inputId (ptr + 4) (replaceAtIndex z (if x == y then 1 else 0) mem) output
  where
    (opcode, [modeX, modeY, _]) = toParams $ mem !! ptr
    x = getByMode (ptr + 1) modeX mem
    y = getByMode (ptr + 2) modeY mem
    z = mem !! (ptr + 3)

computeOutput :: Int -> [Int] -> Int
computeOutput inputId mem = head $ run inputId 0 mem []

first :: String -> Int
first inp = computeOutput 1 $ parse inp

second :: String -> Int
second inp = computeOutput 5 $ parse inp

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ first input
  print $ second input
