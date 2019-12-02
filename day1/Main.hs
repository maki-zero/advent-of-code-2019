getFuel :: Integer -> Integer
getFuel mass = (mass `div` 3) - 2

getInitialFuel :: [String] -> Integer
getInitialFuel [] = 0
getInitialFuel (x:xs) = (getFuel $ read x) + getInitialFuel xs

calculateFuelForMass :: Integer -> Integer
calculateFuelForMass input
    | fuel <= 0 = 0
    | otherwise = fuel + calculateFuelForMass fuel
    where fuel = getFuel input

getTotalFuel :: [String] -> Integer
getTotalFuel [] = 0
getTotalFuel (x:xs) = totalFuel + getTotalFuel xs
    where totalFuel = calculateFuelForMass $ read x

isNotEmpty :: [a] -> Bool
isNotEmpty [] = False
isNotEmpty x = True

main = do
    putStrLn "** Day One **"
    contents <- readFile "./input.txt"
    let fileLines = filter isNotEmpty $ lines contents
    putStr "Total initial fuel: "
    print $ getInitialFuel fileLines
    putStr "Total fuel: "
    print $ getTotalFuel fileLines

