-- Project Euler Problem 10
-- UNSOLVED!!
-- This program runs too long

primes =
    let filterPrimes (p:xs) = p:filterPrimes [x | x <- xs, x `rem` p /= 0]
    in  filterPrimes [2..]

p10 = sum $ takeWhile (\x -> x < 2000000) primes

main = print $ p10
