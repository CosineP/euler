-- Project Euler Problem 7

-- A way to find primes, not the best way
-- The way on Haskell.org, similar to sieve of erestosthenes, is faster
-- But I couldn't remember how and didn't want to look it up
-- So we get this monstrosity
getprimes :: [Int] -> [Int]
getprimes [] = getprimes [2]
getprimes ps =
    let nextprime ps = head $ filter (\x -> prime x ps) [last ps..]
        prime p ps = all (\x -> p `mod` x /= 0) ps
        together = ps ++ [nextprime ps]
    in  nextprime ps:getprimes together

primes = 2:getprimes []

main = print $ primes !! 10000
