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

map = undefined
filter = undefined
reverse = undefined

takeWhile = undefined

minimum = undefined
maximum = undefined
