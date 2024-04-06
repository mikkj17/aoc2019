first :: String -> Int
first inp = error "Not implemented yet"

second :: String -> Int
second inp = error "Not implemented yet"

main :: IO ()
main = do
  testInput <- readFile "test-input.txt"
  input <- readFile "input.txt"
  print $ first input
  print $ second input
