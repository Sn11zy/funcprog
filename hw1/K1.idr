module K1

sumInt : Int -> Int
sumInt 0 = 0
sumInt n = n+ sumInt (n-1)


fib : Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
