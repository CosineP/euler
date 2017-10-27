-- Project Euler Problem 6

p6 :: Int
p6 =
    let square x = x * x
        sumsquares = sum $ map square [1..100]
        squaresums = square $ sum [1..100]
    in  squaresums - sumsquares

main = print p6
