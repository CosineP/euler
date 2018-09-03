-- Not close to done yet, despite appearances
import Data.Time

days = diffDays end start
  where start = fromGregorian 1901 01 01
        end = fromGregorian 2000 12 31

fmonths daysleft month = fmonths (daysleft - gregorianMonthLength month) month + 1

-- 0 = M = start
-- 6 = S
-- (days - 6) / 7
p19 :: Integer
p19 = quot (days + 1) 7

main = print p19
