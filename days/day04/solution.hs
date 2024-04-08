import Data.List (group)
import Data.List.Split (splitOn)

parse :: String -> [Int]
parse inp =
  let parts = splitOn "-" inp
   in [read $ head parts .. read $ last parts]

adjacentSame :: String -> Bool
adjacentSame digits = any (uncurry (==)) $ zip (init digits) (tail digits)

isAscending :: String -> Bool
isAscending digits = all (uncurry (<=)) $ zip (init digits) (tail digits)

notPartOfLargerGroup :: String -> Bool
notPartOfLargerGroup digits = any (\grp -> length grp == 2) $ group digits

compute :: [String -> Bool] -> String -> Int
compute rules inp = length $ filter (\num -> all (\r -> r num) rules) $ map show $ parse inp

first :: String -> Int
first = compute [adjacentSame, isAscending]

second :: String -> Int
second = compute [isAscending, notPartOfLargerGroup]

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ first input
  print $ second input
