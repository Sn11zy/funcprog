filter' : (a -> Bool) -> List a -> List a
filter' _ [] = []
filter' p (x::xs) =[x|p x]++ filter' p xs

nullid1 : List Int -> Int
nullid1 [] = 0
nullid1 (0::xs) = 1+(nullid1 xs)
nullid1 (x::xs) = (nullid1 xs)

nullid2 : List Int -> Int
nullid2 xs = foldr (\x => if x==0 then (+)1 else (+)0) 0 xs

nullid3 : List Int -> Int
nullid3 xs = sum(map (\x => if x==0 then 1 else 0) xs)

nullid4 : List Int -> Nat
nullid4 xs = length(filter' (==0) xs)

nullid5 : List Int -> Int
nullid5 xs = cast(length([x|x<-xs,x==0]))

length' : List a -> Int
length' xs = foldl com 0 xs
  where com : Int -> a -> Int
        com x y = x+1

productList : List Int -> Int
productList xs = foldr (*) 1 xs

{--
productList [3,2,0]
foldr (*) 1 [3,2,0]
3*foldr (*) 1 [2,0]
3*2*foldr (*) 1 [0]
3*2*0*foldr (*) 1 []
3*2*0*1
0
--}

append' : List a -> List a -> List a
append' xs ys = foldr (::) ys xs

isEven : Nat -> Bool
isEven Z         = True
isEven (S Z)     = False
isEven (S (S n)) = isEven n
 
all' : (a -> Bool) -> List a -> Bool
all' p xs = foldr (com p) True xs
  where com : (a -> Bool) -> a -> Bool -> Bool
        com p a lis = (p a) && lis

reverse' : List a -> List a
reverse' = foldl rev df
  where
    df : List a
    df = []
    rev : List a -> a -> List a
    rev x y = y::x

eemaldaNullid : List Int -> List Int
eemaldaNullid = foldr rem df
  where
    df : List Int
    df = []
    rem : Int -> List Int -> List Int
    rem x y = if x==0 then y else x::y

allEqual : List Int -> Bool
allEqual [] = True
allEqual (x::xs) = foldl (com x) True xs
  where com : Int -> Bool -> Int -> Bool
        com x cur n = cur && (x==n)

unzip' : List (a, b) -> (List a, List b)
unzip' = foldr f z
  where
    z : (List a, List b)
    z = ([],[])
    f : (a, b) -> (List a, List b) -> (List a, List b)
    f (a,b) (an,bn) = (a::an,b::bn) 

removeAll1 : Int -> List Int -> List Int
removeAll1 n xs = foldr (com n) [] xs
  where com : Int -> Int ->List Int -> List Int
        com n x li = if x==n then li else x::li

removeAll2 : Int -> List Int -> List Int
removeAll2 n xs = filter (\x => not(n==x)) xs

removeAll3 : Int -> List Int -> List Int
removeAll3 n xs = [x|x<-xs,not(x==n)]

any' : (a -> Bool) -> List a -> Bool
any' p xs = foldl (com p) False xs
  where com : (a -> Bool) -> Bool -> a -> Bool
        com p c x = c||(p x)
