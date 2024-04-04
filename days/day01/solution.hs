toInt :: String -> Integer
toInt s = read s :: Integer

required :: Integer -> Integer
required mass = mass `div` 3 - 2

compute :: (Integer -> Integer) -> String -> Integer
compute f inp = sum $ map (f . toInt) $ lines inp

first :: String -> Integer
first = compute required

fuelRequired :: Integer -> Integer
fuelRequired mass =
  if remaining <= 0
    then remaining
    else remaining + fuelRequired remaining
  where
    remaining = required mass

second :: String -> Integer
second = compute fuelRequired

main :: IO ()
main = do
  testInput <- readFile "test-input.txt"
  input <- readFile "input.txt"
  print $ first input
  print $ second input
