-- Project Euler, Problem 15

-- This is really a pure math problem. The way to get to the bottom is to move
-- 20 to the right and 20 down, no matter what. The order in which you take
-- those 20 steps is the question, and this is a simple permutations problem.
-- The number of ways to order 40 steps (20 down 20 right) with two kinds is
-- 40!/(20!20!)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

p15 = factorial 40 `div` ((factorial 20) * (factorial 20))

main = print $ p15
