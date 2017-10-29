-- Project Euler Problem 12

triangles :: [Int]
triangles = scanl1 (+) [1..]

-- Taken from 3.hs / Problem 3
factorize :: Int -> [Int]
factorize 1 = [1]
factorize x
    | factor == x = [x]
    | otherwise = factor:(factorize $ x `div` factor)
    where factor = head $ filter (\c -> x `mod` c == 0) (2:[3,5..])

-- Given prime factors, find total factors
-- Doesn't count - uses algebraic formula, product of (exponents + 1)
factorcount :: [Int] -> Int
factorcount [] = 1 -- Itself; ie (n)^0, exp+1 = 1
factorcount factorlist = (countfirst + 1) * factorcount rest
    where countfirst = length $ filter (== firstfact) factorlist
          rest = filter (/= firstfact) factorlist
          firstfact = factorlist !! 0

numfactors n = factorcount $ factorize n

p12 = head $ filter (\(n,f) -> f>500) trianglefactors
    where trianglefactors = [(n, numfactors n) | n <- triangles]

main = print $ p12

