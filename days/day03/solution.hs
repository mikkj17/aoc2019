import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Set (fromList, intersection, toList)

type Step = (Char, Int)

type Path = [Step]

type Position = (Int, Int)

type Wire = [Position]

toInt :: String -> Int
toInt s = read s :: Int

parseToPath :: String -> Path
parseToPath row = map (\s -> (head s, toInt $ tail s)) $ splitOn "," row

parse :: String -> (Path, Path)
parse inp =
  let parts = splitOn "\n" inp
   in (parseToPath $ head parts, parseToPath $ last parts)

makeStep :: Position -> Step -> Position
makeStep (y, x) (direction, amount)
  | direction == 'U' = (y + amount, x)
  | direction == 'D' = (y - amount, x)
  | direction == 'L' = (y, x - amount)
  | otherwise = (y, x + amount)

positionsBetween :: Position -> Step -> Wire
positionsBetween pos (direction, amount) = [makeStep pos (direction, i) | i <- [1 .. amount]]

computeWire :: Wire -> Path -> Wire
computeWire wire [] = wire
computeWire wire (x : xs) = wire ++ computeWire (positionsBetween (last wire) x) xs

getWireFromPath :: Path -> Wire
getWireFromPath = computeWire [(0, 0)]

intersectionsBetweenWires :: Wire -> Wire -> [Position]
intersectionsBetweenWires w1 w2 = filter (\x -> x /= (0, 0)) $ toList $ intersection (fromList w1) (fromList w2)

compute :: (Wire -> Wire -> Position -> Int) -> String -> Int
compute f inp = minimum $ map (f w1 w2) $ intersectionsBetweenWires w1 w2
  where
    (p1, p2) = parse inp
    w1 = getWireFromPath p1
    w2 = getWireFromPath p2

first :: String -> Int
first = compute (\_ _ (y, x) -> abs y + abs x)

distanceToIntersection :: Wire -> Wire -> Position -> Int
distanceToIntersection w1 w2 inter = d1 + d2
  where
    d1 = fromJust $ elemIndex inter w1
    d2 = fromJust $ elemIndex inter w2

second :: String -> Int
second = compute distanceToIntersection

main :: IO ()
main = do
  testInput <- readFile "test-input.txt"
  input <- readFile "input.txt"
  print $ first input
  print $ second input
