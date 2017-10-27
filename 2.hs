-- Project Euler Problem 2

fib =
    let latter a b = b:latter b (a + b)
        fibo a b = a:latter a b
    in  fibo 1 1

p2 =
    let evenfib = filter even fib
    in  sum (takeWhile (<= 4000000) evenfib)

main = print $ p2
