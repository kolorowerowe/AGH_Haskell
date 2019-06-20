module Kolos where 


-------------------------------------------------------
-- drzewo wielogałęźne, z głębokością i wartością ustaloną
-- co do ?????


-------------------------------------------------------
-- lista jednokierunkowa 

data List a = Empty | Elem { listHead :: a, listTail :: List a} 
	deriving (Show, Read, Eq, Ord)  

map' :: (t -> a) -> List t -> List a
map' _ Empty = Empty
map' f (Elem head tail) = f head `Elem` (map' f tail) 


foldl' :: (t1 -> t -> t1) -> t1 -> List t -> t1
foldl' f start Empty = start
foldl' f start (Elem head tail) = foldl' f (f start head) tail

foldr' :: (t1 -> t -> t) -> t -> List t1 -> t
foldr' f start Empty = start
foldr' f start (Elem head tail) = f head (foldr' f start tail)

zipWith' :: (t1 -> t -> a) -> List t1 -> List t -> List a
zipWith' _ Empty _ = Empty
zipWith' _ _ Empty = Empty
zipWith' f (Elem h1 t1) (Elem h2 t2) = f h1 h2 `Elem` zipWith' f t1 t2

infixr 5 +++
( +++ ) :: List a -> List a -> List a
Empty +++ Empty = Empty
Empty +++ (Elem h t) = (Elem h t)
(Elem h t) +++ Empty = (Elem h t)
(Elem h1 t1) +++ (Elem h2 t2) = h1 `Elem` (t1 +++ (Elem h2 t2))

fromNormalList :: [a] -> List a
fromNormalList [] = Empty
fromNormalList (x:xs) = x `Elem` fromNormalList xs

toNormalList :: List a -> [a]
toNormalList Empty = []
toNormalList (Elem h t) = h : toNormalList t

-- wyżej już zdefiniowałem map' dla listy. Nie muszę również pisać fmap f Elem = map' f Elem, gdyz wartości zostaną do nich podane
instance Functor List where
	fmap = map' 

instance Applicative List where
	pure a = Elem a Empty
	Empty <*> _ = Empty
	(Elem head tail) <*> something = (fmap head something) +++  (tail <*> something)

------------------------------------------------
-- dane do list
listaInt = 5 `Elem` (3 `Elem` (9 `Elem` Empty))
listaInt2 = 1 `Elem` (5 `Elem` Empty)
listaString = "AAA" `Elem` ("BBB" `Elem` ("CCC" `Elem` Empty))
