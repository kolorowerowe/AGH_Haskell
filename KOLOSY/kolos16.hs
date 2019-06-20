module Kolos where
import Data.Char (ord)


----------------- poniedziałek 8:00

-- zadanie 1
(.) :: (b -> c) -> (a -> b) -> a -> c  
f . g = \x -> f (g x)

-- zadanie 2
tripleInList :: [a] -> [a]
tripleInList [] = []
tripleInList (x:xs) = x : x : x : tripleInList xs 


-- zadanie 3
mainCountWords = do
	input <- getLine
	putStrLn $ show $ countWord input

countWord:: String -> Int
countWord napis = length $ words napis 


-- zadanie 4
data Problem a = Ok a | Error String

instance Functor Problem where
	fmap f (Ok a) = Ok (f a)
	fmap _ (Error a) = Error a

instance Applicative Problem where
	pure a = Ok a
	(Error a) <*> _ = (Error a )
	(Ok a) <*> something = fmap a something

instance Monad Problem where
	return x = Ok x
	Ok a >>= f = f a
	Error a >>= f = Error a
	fail a = Error a


-- zadanie 5 


----------------- poniedziałek 11:00

-- zadanie 2
noWhiteSigns :: String -> String
noWhiteSigns napis = filter (\x -> x /= ' ') napis


-- zadanie 3
map' :: (a -> b) -> [a] -> [b]
map' f list = foldl (\acc x -> acc ++ [f x]) [] list


-- zadanie 4
mainCountSum = do
	input <- getLine
	putStrLn "XD"

sumOfString :: String -> Int
sumOfString "" = 0
sumOfString (x:xs) = ord x - ord '0' + sumOfString xs  


-- zadanie 5



-- zadanie 6
-- data Tree a = Node a [Tree a] 
