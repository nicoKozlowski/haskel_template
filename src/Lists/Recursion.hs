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

last :: [a] -> a
last [x] = x
last (_:xs) = last xs

init :: [a] -> [a]
init [_] = []
init (x:xs) = x : init xs

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y:ys)
    | x == y = True
    | otherwise = elem x ys

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f (x:xs)
    | f x = True
    | otherwise = any f xs

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all f (x:xs)
    | f x = all f xs
    | otherwise = False

maximum :: Ord a => [a] -> a
maximum [x] = x
maximum (x:xs)
    | x > max = x
    | otherwise = max
    where max = maximum xs

minimum :: Ord a => [a] -> a
minimum [x] = x
minimum (x:xs)
    | x < min = x
    | otherwise = min
    where min = minimum xs

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

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
takeWhile f (x:xs)
    | f x = x : takeWhile f xs
    | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f [] = []
dropWhile f (x:xs)
    | f x = dropWhile f xs
    | otherwise = x : xs
     
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

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

