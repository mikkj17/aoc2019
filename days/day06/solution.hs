import Data.List (groupBy, sort)
import Data.List.Split (splitOn)
import Data.Map (Map, findWithDefault, fromList, keys, notMember, union, unionWith, (!))

parse :: ([String] -> [String]) -> String -> Map String [String]
parse transform inp =
  fromList
    . map (\grp -> (head $ head grp, map last grp))
    . groupBy (\x y -> head x == head y)
    . sort
    . map (transform . splitOn ")")
    $ lines inp

countOrbits :: Map String [String] -> Int
countOrbits orbitMap = sum $ map countSingleObject $ keys orbitMap
  where
    countSingleObject :: String -> Int
    countSingleObject o =
      let orbits = findWithDefault [] o orbitMap
       in length orbits + sum (map countSingleObject orbits)

first :: String -> Int
first inp = countOrbits $ parse id inp

bfs :: Map String [String] -> String -> String -> Int
bfs graph root goal = search [root] $ fromList [(root, 0)]
  where
    search :: [String] -> Map String Int -> Int
    search (v : queue) distances =
      let distance = distances ! v
          adjacent = filter (`notMember` distances) (graph ! v)
       in if v == goal
            then distance - 2
            else search (queue ++ adjacent) (distances `union` fromList (map (,distance + 1) adjacent))

second :: String -> Int
second inp =
  let graph = unionWith (++) (parse id inp) (parse reverse inp)
   in bfs graph "YOU" "SAN"

main :: IO ()
main = do
  testInput <- readFile "test-input.txt"
  testInput2 <- readFile "test-input2.txt"
  input <- readFile "input.txt"
  print $ first input
  print $ second input
