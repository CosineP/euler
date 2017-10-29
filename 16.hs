-- Project Euler, Problem 16
-- Luna Phipps-Costin

digits :: [Int]
digits = map (\c -> read [c]) $ show (2^1000)

p16 = sum digits

main = print p16
