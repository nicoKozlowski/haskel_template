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
-- (1) elem[] = False
-- (2) elem x (y:ys) = f x (elem x ys)
-- da elem[] = False ergibt sich in (1) False = s
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
-- f x = (&&)
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
-- f x = (||)
-- or = foldr (||) False

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f (x:xs) = f x || any f xs
-- Universelle Eigenschaft:
-- g[] = s
-- g (x:xs) = f x (g xs)
-- Transformation:
-- g = any
-- (1) any f [] = s
-- (2) any f (x:xs) = f x (any f xs)
-- da any f [] = False ergibt sich in (1) False = s
-- f bestimmen:
-- (2) any f (x:xs) = f x (any f xs)
-- = f x || any f xs = f x (any f xs)
-- f x || n = f x n <- Generalisierung any f xs zu n
-- f = \x n -> f x || n
-- any f = foldr (\x n -> f x || n) False

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all f (x:xs) = f x && all f xs
-- Universelle Eigenschaft:
-- g[] = s
-- g (x:xs) = f x (g xs)
-- Transformation:
-- g = all f
-- (1) all f [] = s
-- (2) all f (x:xs) = f x (all f xs)
-- da all f [] = True ergibt sich in (1) True = s
-- f bestimmen:
-- (2) all f (x:xs) = f x (all f xs)
-- = f x && all f xs = f x (all f xs)
-- f x && n = f x n <- Generalisierung any f xs zu n
-- f = \x n -> f x && n
-- all f = foldr (\x n -> f x && n) False

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
-- concat = foldr (\x n -> x ++ n) []

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
takeWhile f (x:xs) = if f x then x : takeWhile f xs else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f [] = []
dropWhile f (x:xs) = if f x then dropWhile f xs else x : xs
     
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs
-- Universelle Eigenschaft:
-- g[] = s
-- g (x:xs) = f x (g xs)
-- Transformation:
-- g = map f
-- (1) map f [] = s
-- (2) map f (x:xs) = f x (map f xs)
-- da map f [] = [] ergibt sich in (1) [] = s
-- f bestimmen:
-- (2) map f (x:xs) = f x (map f xs)
-- = f x : map f xs = f x (map f xs)
-- f x : n = f x n <- Generalisierung map f xs zu n
-- f = \x n -> f x : n
-- map f = foldr (\x n -> f x : n) []

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs)
    | f x = x : filter f xs
    | otherwise = filter f xs
  
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition _ [] = ([], [])
partition f (x:xs)
    | f x = (x : ts, fs)
    | otherwise = (ts, x : fs)
    where (ts, fs) = partition f xs

