triangle :: [[Int]]
triangle = strings
    where strings = map line $ lines raw
          line from = map read $ words from

-- Given a path, return the two ways I could descend one level
-- each index is either (triangle !! n - 1) or ((triangle !! n - 1) + 1)
split :: [Int] -> [[Int]]
split path = [left,right]
    where left = path ++ [end]
          right = path ++ [end+1]
          end = last path

-- Given all paths, every way to descend one level of the triangle
pathlevel :: [[Int]] -> [[Int]]
pathlevel path = foldl1 (++) (map split path)

-- Given number of levels, give all possible paths through triangle
paths :: Int -> [[Int]]
paths 0 = [[0]]
paths levels = pathlevel $ paths (levels-1)

pathvalues :: [[Int]] -> [[Int]]
pathvalues pathlist = map path [0..(numpaths-1)]
    where path i = [tri r (pathlist !! i !! r) | r <- [0..(depth-1)]]
          tri r c = triangle !! r !! c
          depth = length (pathlist !! 0)
          numpaths = length pathlist

-- Find all paths, find the values on those paths, find the sum of those paths, and find the biggest one
-- Actually I didn't find out why it's length triangle - 1 *shrug*
p18 :: Int
p18 = maximum $ map sum $ pathvalues $ paths $ length triangle - 1

main = print $ p18

raw = "\
\75\n\
\95 64\n\
\17 47 82\n\
\18 35 87 10\n\
\20 04 82 47 65\n\
\19 01 23 75 03 34\n\
\88 02 77 73 07 63 67\n\
\99 65 04 28 06 16 70 92\n\
\41 41 26 56 83 40 80 70 33\n\
\41 48 72 33 47 32 37 16 94 29\n\
\53 71 44 65 25 43 91 52 97 51 14\n\
\70 11 33 28 77 73 17 78 39 68 17 57\n\
\91 71 52 38 17 14 91 43 58 50 27 29 48\n\
\63 66 04 68 89 53 67 30 73 16 69 87 40 31\n\
\04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"
