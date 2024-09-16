import Data.Monoid.Exponentiation

fst' : (a, b) -> a
fst' (a, b) = a

length' : List a -> Int
length' [] = 0
length' (_::xs) = 1 + length' xs
{--
length' [2, 3, 4]
1+length' [3, 4]
1+1+length' [4]
2+1
3
--}

infixr 7 +++
(+++) : List a -> List a -> List a
(+++) [] b = b
(+++) (x::xs) b = x::((+++) xs b)
{--
[1] +++ [2]
1::([] +++ [2])
1::[2]
[1, 2]
--}


replicate' : Int -> a -> List a
replicate' 0 b = []
replicate' a b = b::(replicate' (a-1) b)

take' : Int -> List a -> List a
take' 0 a = []
take' n [] = []
take' n (x::xa) = x::(take' (n-1) xa)

sum' : List Integer -> Integer
sum' [] = 0
sum' (x::xa)= x + (sum' xa)

drop' : Int -> List a -> List a
drop' 0 a = a
drop' n [] = []
drop' n (x::xa) = drop' (n-1) xa 


reverse' : List a -> List a
reverse' [] = []
reverse' (x::xs) = (reverse' xs) +++ [x]

esimesed : List (a, b) -> List a
esimesed [] = []
esimesed ((a,b)::xs) = a::(esimesed xs)

leidub : Integer -> List Integer -> Bool
leidub n []= False
leidub n (x::xs) = if x == n then True else (leidub n xs)

dropLast : List a -> List a
dropLast [] = []
dropLast (x::[]) = []
dropLast (x::xs) = x::(dropLast xs)

lisa' : Int -> Char -> List Char -> List Char
lisa' i x [] = [x]
lisa' i x (y::ys) =
  case i<=0 of
      True => x::(y::ys)
      False => y::lisa' (i-1) x ys


lisa : Int -> Char -> String -> String
lisa i x ys = pack(lisa' i x (unpack ys))



arvuta : List (Double, Nat) -> Double -> Double
arvuta [] x = 0
arvuta ((d,n)::sp) x = d*x^n + (arvuta sp x )

