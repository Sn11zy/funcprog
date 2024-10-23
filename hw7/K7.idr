{--
add 1 1 = (λm n. λf x. m f (n f x)) 1 1
-> λf x. 1 f (1 f x) 
-> λf x. f (1 f x) 
-> λf x. f (f x)
= 3

add 0 0 = (λm n. λf x. m f (n f x)) 0 0
-> λf x. 0 f (0 f x)
-> λf x. 0 f x
-> λf x. x
= 0

mul 2 0 = (λm n. λf x. m (n f) x) 2 0
-> λf x. 2 (0 f) x
-> λf x. 2 x
-> λf x. x
=0

mul 2 3 = (λm n. λf x. m (n f) x) 2 3
-> λf x. 2 (3 f) x
-> λf x. 2 (f(f(f()))) x
-> λf x. f(f(f(f(f(f x)))))
=6

exp 2 2 = (λm n. λf x. n m f x) 2 2
-> λf x. 2 2 f x
-> λf x. 2 f(f x)
-> λf x. f(f(f(f x)))
= 4

exp 2 0 = (λm n. λf x. n m f x) 2 0
-> λf x. 0 2 f x
-> λf x. 0 (f(f x)
-> λf x. x
=0
--}



import Data.List
 
data Tree k v = E | T (Tree k v) k v (Tree k v)
 
test124 : Tree Int String
test124 = T (T E 1 "\252ks" (T E 2 "kaks" E)) 4 "neli" E
 
treeToList : Tree k v -> List (k,v)
treeToList E = []
treeToList (T x y z w) = (y,z) :: treeToList x ++ treeToList w

(Eq k, Eq v) => Eq (Tree k v) where
  E == E = True
  (T l k v r) == (T l1 k1 v1 r1) = (l==l1) && (k==k1) && (v==v1) && (r==r1)
  _ == _ = False
 

lookup : Ord k => k -> Tree k v -> Maybe v
lookup k E = Nothing
lookup s (T l k v r) = if k == s then Just v else if s<k then lookup s l else lookup s r

insert : Ord k => k -> v -> Tree k v -> Tree k v
insert k v E = T E k v E
insert ik iv (T l k v r) = case (compare ik k) of
  EQ => T l k iv r
  LT => T (insert ik iv l) k v r
  GT => T l k v (insert ik iv r)

fold : Ord k => (k -> v -> a -> a) -> a -> Tree k v -> a
fold f a E = a
fold f a (T l k v r) = fold f (f k v (fold f a r)) l


insertAll : Ord k => List (k,v) -> Tree k v -> Tree k v
insertAll [] t = t
insertAll ((ik,iv)::xs) t = (insertAll xs (insert ik iv t))

combine : Ord k => Tree k v -> Tree k v -> Tree k v
combine t (T E k1 v1 r1) = (T t k1 v1 r1)
combine t (T l k v r) = combine t l
combine t E = t

remove : Ord k => k -> Tree k v -> Tree k v
remove _ E = E
remove k1 (T l k v r) = case (compare k1 k) of
  EQ => combine l r 
  LT => T (remove k1 l) k v r
  GT => T l k v (remove k1 r)

union : Ord k => Tree k v -> Tree k v -> Tree k v
union t1 t2 = fold (\k,v,a=>insert k v a) t1 t2 

Set : Type -> Type
Set a = Tree a ()
 
empty : Set a
empty = E
 
setToList : Ord a => Set a -> List a
setToList = fold (\x,(),xs=>x::xs) []

add : Ord a => a -> Set a -> Set a
add a s = insert a () s

delete : Ord a => Set a -> Set a -> Set a
delete s1 s2 = fold (\k,v,a=>remove k a) s1 s2

intersect : Ord a => Set a -> Set a -> Set a
intersect s1 s2 = delete (delete (union s1 s2) (delete s1 s2)) (delete s2 s1)
