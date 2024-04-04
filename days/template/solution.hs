first :: String -> Int
first inp = 0

second :: String -> Int
second inp = 0

main :: IO ()
main = do
  testInput <- readFile "test-input.txt"
  input <- readFile "input.txt"
  print $ first input
  print $ second input
