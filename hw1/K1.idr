module K1

sumInt : Int -> Int
sumInt 0 = 0
sumInt n = n+ sumInt (n-1)


fib : Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
{-
redutseeritud fib3
fib 3
fib 2 + fib 1
fib 1 + fib 0 + fib 1
1 + 0 + 1
2
-}

modulo : Int -> Int -> Int
modulo x y = if (x<y) then x else modulo (x-y) y

syt : Int -> Int -> Int
syt x 0 = x
syt x y = syt y (mod x y)
{-
redutseeritud syt 12 8
syt 12 8
syt 8 (mod 12 8)
syt 8 4
syt 4 (mod 8 4)
syt 4 0
4
-}

hanoi : Int -> Int
hanoi 1 = 1
hanoi n = 2 * hanoi (n-1) + 1

ack : Int -> Int -> Int
ack 0 n = n+1
ack m n = if (m>0 && n==0) then ack (m-1) 1 else ack (m-1) (ack m (n-1))

aste : Int -> Int -> Int
aste x 0 = 1
aste x y = x * aste x (y-1)

qaste : Int -> Int -> Int
qaste x 0 = 1
qaste x n =
  let y : Int
      y = qaste x (n `div` 2)
  in if (n `mod` 2==0) then y*y else x*y*y

ndiv : Int -> Int -> Int
ndiv x y =
  let f : Int -> Int -> Int
      f n z = if (n<y) then z else f (n-y) (z+1)
  in f x 0

korda : Int -> (Int -> Int) -> Int -> Int
korda 0 f x = x
korda n f x = f (korda (n - 1) f x)
 
inc : Int -> Int
inc x = x + 1

add : Int -> Int -> Int
add x y = korda y inc x

mul : Int -> Int -> Int
mul x y = korda y (add x) 0

mc : Int -> Int
mc n = if (n>100) then n - 10 else mc (mc (n+11))

c : Int -> Int -> Int
c n k = if (k==0 || (n == k)) then 1 else if (1<=k && k<=n-1) then c (n-1) (k-1) + c (n-1) k else 0
