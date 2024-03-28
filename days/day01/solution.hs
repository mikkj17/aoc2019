toInt :: String -> Integer
toInt s = read s :: Integer

required :: Integer -> Integer
required mass = mass `div` 3 - 2

first :: String -> Integer
first inp = sum $ map (required . toInt) $ lines inp

fuelRequied :: Integer -> Integer
fuelRequied mass = 
    if remaining <= 0
        then remaining
        else remaining + fuelRequied remaining
        where remaining = required mass

second :: String -> Integer
second inp = sum $ map (fuelRequied . toInt) $ lines inp

main :: IO ()
main = do
    testInput <- readFile "test-input.txt"
    input <- readFile "input.txt"
    print $ first input
    print $ second input
