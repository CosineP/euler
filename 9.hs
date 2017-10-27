-- Project Euler Problem 9

triple :: (Int,Int,Int)
triple =
    let pythag (a,b,c) = (a*a) + (b*b) == (c*c)
        getc (a,b) = 1000 - a - b
        flatten = concatMap id
        abs = flatten $ map (\a -> [(a,b) | b <- [1..1000]]) [1..1000]
        abcs = map (\ab@(a,b) -> (a,b,getc ab)) abs
    in  head $ filter pythag abcs

p9 = let (a,b,c) = triple in a*b*c

main = print $ p9
