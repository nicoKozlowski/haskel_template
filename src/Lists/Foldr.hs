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

and = undefined
or  = undefined

any = undefined
all = undefined

append = undefined
concat = undefined

map = undefined
filter = undefined
reverse = undefined

takeWhile = undefined

minimum = undefined
maximum = undefined
