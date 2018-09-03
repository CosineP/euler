-- Project Euler Problem 331
-- Note: This isn't finished. It is not yet performant enough, nor does it
-- accurately describe the LOWEST number of moves needed

import Data.Array
import Foreign.Marshal.Utils

cleanBoard :: Int -> [[Bool]]
cleanBoard n = replicate n $ replicate n False

-- As defined by the problem, define a board n x n
initialBoard :: Int -> [[Bool]]
initialBoard n = map valueRow $ labeled $ cleanBoard n
    where cond x y = (n-1) <= hyp x y && hyp x y < n
          hyp x y = truncate $ sqrt $ fromIntegral $ (x*x) + (y*y)
          valueRow row = map value row
          value (_,x,y) = cond x y

-- Take a board and flip the values in a cross pattern
-- This is called a "flip" in the problem
-- Note that this is simultaneously something you do on a board, and something
-- you do on moves to compute which ones are actually necessary
cross :: [[Bool]] -> Int -> Int -> [[Bool]]
cross board x y = map crossRow $ labeled board
    where crossVal (val,a,b) = if x == a || y == b then not val else val
          crossRow row = map crossVal row

-- Print a board cleanly
stringify :: [[Bool]] -> [Char]
stringify board = foldl1 (++) $ map row board
    where row what = map char what ++ "\n"
          char bool = if bool then 'x' else ' '

-- Change from val to (val,x,y) for easier computation
labeled :: [[Bool]] -> [[(Bool, Int, Int)]]
labeled board = [labeledRow row y | (row,y) <- zip board [0..]]
    where labeledRow row y = [(val,x,y) | (val,x) <- zip row [0..]]

-- Labeled, the structure is actually unnecessary and sometimes gets in the way
-- Does *not* label, you might want falattened labeled board
flattened :: [[a]] -> [a]
flattened board = foldl1 (++) board

-- When you need to change ONE singular X, you can do so by crossing in a cross
-- pattern around the X
-- This is not necessary to solve the problem since I know it works, but can be
-- used for demonstration / testing maybe? Lol I didn't need to write it oops
crossCross :: Int -> Int -> [[Bool]] -> [[Bool]]
crossCross x y board = foldl crossIf board indices
    where crossIf acc (a,b) = if x == a || y == b then cross acc a b else acc
          indices = [(a,b) | a <- [0..length board - 1], b <- [0..(length $ board !! 0) - 1]]

neededMoves board = foldl crossOnNew (cleanBoard size) xs
    where xs = filter (\(val,x,y) -> val) $ flattened $ labeled board
          size = length board
          crossOnNew acc (_,x,y) = cross acc x y

movesCount board = sum $ map fromBool flat
    where flat = flattened $ neededMoves board

summation = sum [exp n | n <- [3..31]]
    -- Seemingly arbitrary, given by problem
    where exp n = movesCount $ initialBoard $ 2^n - n

p331 = summation

main = do
    putStrLn $ stringify $ initialBoard 5
    putStrLn $ stringify $ neededMoves $ initialBoard 5
    print $ movesCount $ initialBoard 5
    putStrLn $ stringify $ crossCross 3 4 $ cross (cleanBoard 10) 2 3
    -- print $ p331

