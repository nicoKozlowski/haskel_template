{-# LANGUAGE ExtendedDefaultRules #-}
module Lists.Foldr where
import Prelude hiding (sum,or,filter,
                       prod,any,reverse,
                       length,all,takeWhile,
                       elem,concat,minimum,
                       and,map,maximum)

sum = foldr (+) 0

prod = foldr (*) 1

length = foldr (\_ res -> 1 + res) 0

elem x = foldr (\y res -> y == x || res) False

and = foldr (&&) True
or  = foldr (||) False

any f = foldr (\x res -> f x) False
all f = foldr (\x res -> f x && res) True

append xs ys = foldr (:) ys xs
concat = foldr (++) []

map f = foldr (\x res -> f x : res) []
filter f = foldr (\x res -> if f x then x : res else res) []
reverse xs = foldr (\x res -> res ++ [x]) [] xs

takeWhile f = foldr (\x res -> if f x then x : res else []) []

minimum xs = foldr (\x res -> if x < res then x else res) (head xs) xs
maximum xs = foldr (\x res -> if x > res then x else res) (head xs) xs
