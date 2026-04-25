{-# LANGUAGE ExtendedDefaultRules #-}
module Lists.Foldr where
import Prelude hiding (sum,or,filter,
                       prod,any,reverse,
                       length,all,takeWhile,
                       elem,concat,minimum,
                       and,map,maximum)
sum :: Num a => [a] -> a
sum = foldr (+) 0

prod :: Num a => [a] -> a
prod = foldr (*) 1

length :: [a] -> Int
length = foldr (\_ n -> 1 + n) 0

elem :: Eq a => a -> [a] -> Bool
elem x = foldr (\y n -> y == x || n) False

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or  = foldr (||) False

any :: (a -> Bool) -> [a] -> Bool
any f = foldr (\x n -> f x || n) False

all :: (a -> Bool) -> [a] -> Bool
all f = foldr (\x n -> f x && n) True

append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x n -> f x : n) []

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x n -> if f x then x : n else n) []

reverse :: [a] -> [a]
reverse xs = foldr (\x n -> n ++ [x]) [] xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f = foldr (\x n -> if f x then x : n else []) []

minimum :: Ord a => [a] -> a
minimum xs = foldr (\x n -> if x < n then x else n) (head xs) xs

maximum :: Ord a => [a] -> a
maximum xs = foldr (\x n -> if x > n then x else n) (head xs) xs
