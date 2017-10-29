-- Project Euler, Problem 14

collatzlen :: Int -> Int
collatzlen n = rcollatzlen n 1
    where rcollatzlen n l
            | even n = rcollatzlen (n `div` 2) (l+1)
            | n == 1 = l
            | otherwise = rcollatzlen (3*n + 1) (l+1)

p14 = maximum starts
    where starts = [(collatzlen n, n) | n <- [1..1000000]]

main = print p14
