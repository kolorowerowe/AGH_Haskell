module Kolos where

import Data.List

data Pizza = Pizza {
	pizzaId :: Int,
	pizzaName :: String,
	price :: Double,
	size :: Int
}

data Topping = Topping {
	ofPizzaId :: Int,
	topName :: String,
	veggie :: Bool,
	alergens :: [String]
}

--------------------------------------------------------------------
-- 1 zadanie 
data PizzaWithToppings = PizzaWithToppings{
	pizza :: Pizza,
	listOfToppings :: [Topping]
}


-- 2 zadanie
instance Show Topping where
	show (Topping id name veg al) = name ++ " on pizza " ++ show id  ++  (if veg then " is veggie" else " is not veggie") ++ " and have these alergens: " ++ unwords al


-- 3 zadanie
instance Eq Pizza where
	(Pizza i n p s) == (Pizza i' n' p' s') = (i==i') && (n==n') && (p==p') && (s==s')


-- 4 zadanie
-- 1)
sortByPrice :: [Pizza] -> [Pizza]
sortByPrice = sortBy (\(Pizza _ _  p1 _ ) (Pizza _ _  p2 _ ) -> compare p1 p2)

-- 2) 
sortByNameDescThenSize :: [Pizza] -> [Pizza]
sortByNameDescThenSize = sortBy (\(Pizza _ n1 _ _ ) (Pizza _ n2 _ _ ) -> compare n2 n1) . sortBy (\(Pizza _ _ _ s1 ) (Pizza _ _ _ s2 ) -> compare s1 s2)

sortByAlergens :: [Topping] -> [Topping]
sortByAlergens= sortBy (\(Topping _ _ _ al1) (Topping _ _ _ al2) -> compare (length al1) (length al2))


-- 5 zadanie
toPizzaWithToppings :: Pizza -> Topping -> Maybe PizzaWithToppings
toPizzaWithToppings (Pizza i n p s ) (Topping id na ve al)
	| i == id = Just (PizzaWithToppings (Pizza i n p s ) [Topping id na ve al] )
	| otherwise = Nothing


-- 6 zadanie
findById :: [Topping] -> Int -> [Topping]
findById ls nr = filter (\(Topping id _ _ _ ) -> id == nr) ls 


-- 7 zadanie 
mapBy f = map f pizzas
-- teraz sprawdzamy za pomocą ghci> :t mapBy
mapBy :: (Pizza -> b) -> [b]


-- 8 zadanie 
reduceBy1 f = foldl f [("napis",0)] "dlugi napis..."
reduceBy1 :: Num t => ([([Char], t)] -> Char -> [([Char], t)]) -> [([Char], t)]
-- t musi być numerowane =>  ( funkcja, przyjmująca listę krotek(Char,t) i Chara, która zwraca listę krotek(Char,t) ) zwraca  listę krotek(Char,t) 


-- 9 zadanie 
-- reduceBy2 f = foldl f Data.Set.empty [5,6,1,6]
-- reduceBy2 :: Num a => (Set a1 -> a -> Set a1) -> Set a1
-- tutaj wszystko jest w komentarzach, bo importowanie biblioteki Data.Set daje kolejne funkcje filter i foldl, których używaliśmy wcześniej i kompilator nie wie których ma użyć. Przetestowałem to rozwiązanie w osobnym pliku.

-- 10 zadanie 
join :: [Pizza] -> [Topping] -> [PizzaWithToppings]
join pizzaList toppingList = map (\(Pizza id n p s ) -> (PizzaWithToppings (Pizza id n p s ) (findById toppingList id) ) ) pizzaList





------------------------------------------------------------------
-- przykładowe dane Wypca
pizzas :: [Pizza]
pizzas = [Pizza 26543 "Pleisticenska" 32.8 42, Pizza 33596 "Kebab" 29.9 42]

toppings :: [Topping]
toppings = [Topping 33596 "Ser Mozarella" False ["Laktoza", "Mleko"], Topping 26543 "Zielone Soczyste Brokuły" True []]






------------------------------------------------------------------
-- funkcje pomocnicze
instance Show Pizza where
	show (Pizza i n p s) =  n ++ ", id: " ++ show i

instance Show PizzaWithToppings where
	show (PizzaWithToppings pizza topping) = "PIZZA: " ++ show pizza ++ " ZE SKŁADNIKAMI: " ++ show topping



------------------------------------------------------------------
-- przykładowe moje dane

pizza1 = Pizza { pizzaId = 1, pizzaName = "Capriciosa", price = 28.0, size = 30 } 
pizza2 = Pizza { pizzaId = 1, pizzaName = "Capriciosa", price = 28.0, size = 30 } 
pizza3 = Pizza { pizzaId = 3, pizzaName = "Margharita", price = 22.0, size = 28 } 

topping1 = Topping{ ofPizzaId = 1, topName = "Chicken", veggie = False, alergens = ["meat","gluten"]}


