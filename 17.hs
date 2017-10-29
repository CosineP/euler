-- Project Euler, Problem 17
-- Luna Phipps-Costin

import Text.Numeral.Grammar ( defaultInflection )
import Text.Numeral.Language.ENG
import Data.Maybe (fromJust)
import Data.Text (unpack)

numwordlen :: Int -> Int
numwordlen n = adjusted
    where raw = unpack $ fromJust $ gb_cardinal defaultInflection n
          rawlen = length $ filter (`elem` ['a'..'z']) raw
          -- This checks if "and" is in the numeral
          -- Have to do this stupid, STUPID shit because the numeral lib,
          -- DESPITE CLAIMING TO INCLUDE "AND"s, does NOT.
          -- So I manuall check if I'm in the hundreds, and not an exact hundred multiple
          -- In that case, I add three for "and"
          adjusted = if n `mod` 1000 > 100 && n `mod` 100 /= 0 then rawlen + 3 else rawlen

p17 = sum $ map numwordlen [1..1000]

-- raw n = unpack $ fromJust $ gb_cardinal defaultInflection n

main = print $ p17


