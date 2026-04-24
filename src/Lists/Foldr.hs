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
length = foldr (\_ res -> 1 + res) 0

elem :: Eq a => a -> [a] -> Bool
elem x = foldr (\y res -> y == x || res) False

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or  = foldr (||) False

any :: (a -> Bool) -> [a] -> Bool
any f = foldr (\x res -> f x) False

all :: (a -> Bool) -> [a] -> Bool
all f = foldr (\x res -> f x && res) True

append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x res -> f x : res) []

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x res -> if f x then x : res else res) []

reverse :: [a] -> [a]
reverse xs = foldr (\x res -> res ++ [x]) [] xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f = foldr (\x res -> if f x then x : res else []) []

minimum :: Ord a => [a] -> a
minimum xs = foldr (\x res -> if x < res then x else res) (head xs) xs

maximum :: Ord a => [a] -> a
maximum xs = foldr (\x res -> if x > res then x else res) (head xs) xs
