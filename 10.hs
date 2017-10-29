-- Project Euler Problem 10
-- I would like to do this on my own at some point
-- But it's the problem of spoilers: I saw the existing solutions,
-- and knew that I wouldn't be able to solve it myself after that

-- Literally just copied and pasted from
-- https://wiki.haskell.org/Prime_numbers#Postponed_Filters
-- Because I don't really know enough to make a fast prime finder
-- My own prime finder was way too slow. Way.
primes = 2 : oddprimes
    where 
    oddprimes = sieve [3,5..] 9 oddprimes
    sieve (x:xs) q ps@ ~(p:pt)
        | x < q = x : sieve xs q ps
        | True  =     sieve [x | x <- xs, rem x p /= 0] (head pt^2) pt

p10 = sum $ takeWhile (\x -> x < 2000000) primes

main = print $ p10
