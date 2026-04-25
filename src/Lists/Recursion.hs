module Lists.Recursion where
import Prelude hiding (sum,all,reverse,
                       prod,append,take,
                       length,concat,drop,
                       elem,map,takeWhile,
                       and,filter,dropWhile,
                       or,init,minimum,
                       any,last,maximum)

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs
-- Universelle Eigenschaft:
-- g[] = s
-- g (x:xs) = f x (g xs)
-- Transformation:
-- g = length
-- (1) length[] = s
-- (2) length (x:xs) = f x (length xs)
-- da length[] = 0 ergibt sich in (1) 0 = s
-- f bestimmen:
-- (2) length (x:xs) = f x (length xs)
-- = 1 + length xs = f x (length xs)
-- 1 + n = f x n <- Generalisierung length xs zu n
-- f = \x n -> 1 + n
-- length = foldr (\x n -> 1 + n) 0

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs
-- Universelle Eigenschaft:
-- g[] = s
-- g (x:xs) = f x (g xs)
-- Transformation:
-- g = sum
-- (1) sum[] = s
-- (2) sum (x:xs) = f x (sum xs)
-- da sum[] = 0 ergibt sich in (1) 0 = s
-- f bestimmen:
-- (2) sum (x:xs) = f x (sum xs)
-- = x + sum xs = f x (sum xs)
-- x + y = f x y <- Generalisierung sum xs zu y
-- f = (+)
-- sum = foldr (+) 0

prod :: Num a => [a] -> a
prod [] = 1
prod (x:xs) = x * prod xs
-- Universelle Eigenschaft:
-- g[] = s
-- g (x:xs) = f x (g xs)
-- Transformation:
-- g = prod
-- (1) prod[] = s
-- (2) prod (x:xs) = f x (prod xs)
-- da prod[] = 1 ergibt sich in (1) 1 = s
-- f bestimmen:
-- (2) prod (x:xs) = f x (prod xs)
-- = x * prod xs = f x (prod xs)
-- x * y = f x y <- Generalisierung prod xs zu y
-- f = (*)
-- prod = foldr (*) 1

last :: [a] -> a
last [x] = x
last (_:xs) = last xs

init :: [a] -> [a]
init [_] = []
init (x:xs) = x : init xs

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y:ys) = (x == y) || elem x ys
-- Universelle Eigenschaft:
-- g[] = s
-- g (x:xs) = f x (g xs)
-- Transformation:
-- g = elem x
-- (1) elem x [] = False
-- (2) elem x (y:ys) = f x (elem x ys)
-- da elem x [] = False ergibt sich in (1) False = s
-- f bestimmen:
-- (2) elem x (y:ys) = f x (elem x ys)
-- = (x == y) || elem x ys = f x (elem x ys)
-- (x == y) || n = f x n <- Generalisierung elem x ys zu n
-- f x = \y n -> (x == y) || n
-- elem x = foldr (\y n -> (x == y) || n) False

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs
-- Universelle Eigenschaft:
-- g[] = s
-- g (x:xs) = f x (g xs)
-- Transformation:
-- g = and
-- (1) and[] = True
-- (2) and (x:xs) = f x (and xs)
-- da and[] = True ergibt sich in (1) True = s
-- f bestimmen:
-- (2) and (x:xs) = f x (and xs)
-- = x && and xs = f x (and xs)
-- x && n = f x n <- Generalisierung and xs zu n
-- f = (&&)
-- and = foldr (&&) True

or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs
-- Universelle Eigenschaft:
-- g[] = s
-- g (x:xs) = f x (g xs)
-- Transformation:
-- g = or
-- (1) or[] = False
-- (2) or (x:xs) = f x (or xs)
-- da or[] = False ergibt sich in (1) False = s
-- f bestimmen:
-- (2) or (x:xs) = f x (or xs)
-- = x || or xs = f x (or xs)
-- x || n = f x n <- Generalisierung or xs zu n
-- f = (||)
-- or = foldr (||) False

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x:xs) = p x || any p xs
-- Universelle Eigenschaft:
-- g[] = s
-- g (x:xs) = f x (g xs)
-- Transformation:
-- g = any p
-- (1) any p [] = s
-- (2) any p (x:xs) = f x (any p xs)
-- da any p [] = False ergibt sich in (1) False = s
-- f bestimmen:
-- (2) any p (x:xs) = f x (any p xs)
-- = p x || any p xs = f x (any p xs)
-- p x || n = f x n <- Generalisierung any p xs zu n
-- f = \x n -> p x || n
-- any p = foldr (\x n -> p x || n) False

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x:xs) = p x && all p xs
-- Universelle Eigenschaft:
-- g[] = s
-- g (x:xs) = f x (g xs)
-- Transformation:
-- g = all p
-- (1) all p [] = s
-- (2) all p (x:xs) = f x (all p xs)
-- da all p [] = True ergibt sich in (1) True = s
-- f bestimmen:
-- (2) all p (x:xs) = f x (all p xs)
-- = p x && all p xs = f x (all p xs)
-- p x && n = f x n <- Generalisierung all p xs zu n
-- f = \x n -> p x && n
-- all p = foldr (\x n -> p x && n) True

maximum :: Ord a => [a] -> a
maximum [x] = x
maximum (x:xs) = max x (maximum xs)

minimum :: Ord a => [a] -> a
minimum [x] = x
minimum (x:xs) = min x (minimum xs)

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs
-- Universelle Eigenschaft:
-- g[] = s
-- g (x:xs) = f x (g xs)
-- Transformation:
-- g = concat
-- (1) concat[] = s
-- (2) concat (x:xs) = f x (concat xs)
-- da concat[] = [] ergibt sich in (1) [] = s
-- f bestimmen:
-- (2) concat (x:xs) = f x (concat xs)
-- = x ++ concat xs = f x (concat xs)
-- x ++ n = f x n <- Generalisierung concat xs zu n
-- f = (++)
-- concat = foldr (++) []

take :: Int -> [a] -> [a]
take n _
    | n <= 0 = []
take _ [] = []
take n (x:xs) = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop n xs
    | n <= 0 = xs
drop n [] = []
drop n (_:xs) = drop (n - 1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) = if p x then x : takeWhile p xs else []
-- Universelle Eigenschaft:
-- g[] = s
-- g (x:xs) = f x (g xs)
-- Transformation:
-- g = takeWhile p
-- (1) takeWhile p [] = s
-- (2) takeWhile p (x:xs) = f x (takeWhile p xs)
-- da takeWhile p [] = [] ergibt sich in (1) [] = s
-- f bestimmen:
-- (2) takeWhile p (x:xs) = f x (takeWhile p xs)
-- = if p x then x : takeWhile p xs else  [] = f x (takeWhile p xs)
-- if p x then x : n else [] = f x n <- Generalisierung takeWhile p xs zu n
-- f p = \x n -> if p x then x : n else []
-- takeWhile p = foldr (\x n -> if p x then x : n else []) []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:xs) = if p x then dropWhile p xs else x : xs
     
map :: (a -> b) -> [a] -> [b]
map p [] = []
map p (x:xs) = p x : map p xs
-- Universelle Eigenschaft:
-- g[] = s
-- g (x:xs) = f x (g xs)
-- Transformation:
-- g = map p
-- (1) map p [] = s
-- (2) map p (x:xs) = f x (map p xs)
-- da map p [] = [] ergibt sich in (1) [] = s
-- f bestimmen:
-- (2) map p (x:xs) = f x (map p xs)
-- = p x : map p xs = f x (map p xs)
-- p x : n = f x n <- Generalisierung map p xs zu n
-- f = \x n -> p x : n
-- map p = foldr (\x n -> p x : n) []

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs
-- Universelle Eigenschaft:
-- g[] = s
-- g (x:xs) = f x (g xs)
-- Transformation:
-- g = filter p
-- (1) filter p [] = s
-- (2) filter p (x:xs) = f x (filter p xs)
-- da filter p [] = [] ergibt sich in (1) [] = s
-- f bestimmen:
-- (2) filter p (x:xs) = f x (filter p xs)
-- = if p x then x : filter p xs else filter p xs = f x (filter p xs)
-- if p x then x : n else n = f x n <- Generalisierung filter p xs zu n
-- f p = \x n -> if p x then x : n else n
-- filter p = foldr (\x n -> if p x then x : n else n) []

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]
-- Universelle Eigenschaft:
-- g[] = s
-- g (x:xs) = f x (g xs)
-- Transformation:
-- g = reverse
-- (1) reverse [] = s
-- (2) reverse (x:xs) = f x (reverse xs)
-- da reverse [] = [] ergibt sich in (1) [] = s
-- f bestimmen:
-- (2) reverse (x:xs) = f x (reverse xs)
-- = reverse xs ++ [x] = f x (reverse xs)
-- n ++ [x] = f x n <- Generalisierung reverse xs zu n
-- f = \x n -> n ++ [x]
-- reverse = foldr (\x n -> n ++ [x]) []

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition _ [] = ([], [])
partition p (x:xs)
    | p x = (x : ts, fs)
    | otherwise = (ts, x : fs)
    where (ts, fs) = partition p xs

