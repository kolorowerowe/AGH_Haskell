module Kolos where

---------------------------

-- zadanie 1
-- ($) :: (a -> b) -> a -> b  
-- f $ x = f x  
-- $ ma bardzo niski priorytet


-- zadanie 2




-- zadanie 3
mainOdwroc = do
	napis1 <- getLine
	napis2 <- getLine
	napis3 <- getLine
	putStrLn $  reverse napis3 ++ "\n" ++ reverse napis2 ++ "\n" ++ reverse napis1


-- zadanie 4


-- zadanie 5
data Tree a = E | L a | N (Tree a) a (Tree a)
	deriving (Show)

lrv :: Tree a -> [a]
lrv E = []
lrv (L a) = [a]
lrv (N l v r) = lrv l ++ lrv r ++ [v]

drzewo = N (N (L 3) 5 E) 9 (N (L 10) 12 (L 15))

-- zadanie 5