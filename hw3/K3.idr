{--
vabad muutujad
1. {x}
2. {x}
3. {x}
4. {y,g}
5. {z}
--}

{--
substitutsioon
1. (λf. f y(λx.x))[y→λx y. f x] = (λa. a (λx y. f x)(λx.x))
2. (λx. f (x x))(λx. f (x x))[f→λx y. y] = (λx. (λx y. y) (x x))(λx. (λx y. y) (x x))
3. (λx g y.x g y)[g→x g y] = (λa z b.a (x g y) b)
--}

mod7 : List Int
mod7 = [0,7..1000]

count : Char -> String -> Nat
count c s = length ([x|x<-unpack s,x==c])

concat' : List (List a) -> List a
concat' xss = [x|a<-xss,x<-a]

factors : Int -> List Int
factors n = [x|x<-[1..n], mod n x == 0]

isPrime : Int -> Bool
isPrime n = factors n == [1,n]

primes : Int -> List Int
primes n = [x|x<-[1..n-1],(isPrime x)]

zip' : List a -> List b -> List (a,b)
zip' [] b = []
zip' a [] = []
zip' (x::xs) (y::ys) = (x,y)::(zip' xs ys)

pairs : List a -> List (a,a)
pairs [] = []
pairs (x::xs) = zip' (x::xs) [a|a<-xs]

and' : List Bool -> Bool
and' [] = True
and' (x::xs) = x&&and' xs

sorted  : List Int -> Bool
sorted x = and' [z|(a,b)<-pairs x,let z = a<b]

pyths : Int -> List (Int,Int,Int)
pyths n = [(x,y,z)|x<-[1..n],y<-[1..n],z<-[1..n],x*x+y*y==z*z]

perfects : Int -> List Int
perfects n = [x|x<-[1..n],(sum (factors x))-x == x]

