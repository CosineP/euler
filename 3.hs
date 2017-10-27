-- Project Euler Problem 3

factorize :: Int -> [Int]
factorize x
    | factor == x = [x]
    | otherwise = factor:(factorize $ x `div` factor)
    where factor = head $ filter (\c -> x `mod` c == 0) (2:[3,5..])

p3 = last $ factorize 600851475143

main = print $ p3

