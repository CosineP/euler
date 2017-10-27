-- Project Euler Problem 1

p1 =
    let multiple x = x `mod` 5 == 0 || x `mod` 3 == 0
        nums = filter multiple [1..999]
	in  sum nums

main = print p1
