-- Project Euler Problem 4

palindrome :: Int -> Bool
palindrome num
    | backwards == forwards = True
    | otherwise = False
    where forwards = show num
          backwards = reverse forwards

possible :: [Int]
possible =
    let flatten = concatMap id
    in  flatten $ map (\x -> map (*x) [100..999]) [100..999]

palindromes = filter palindrome possible

p4 = maximum palindromes

main = print $ p4
