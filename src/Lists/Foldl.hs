{-# LANGUAGE ExtendedDefaultRules #-}
module Lists.Foldl where
import Prelude hiding (sum,and,last,
                       prod,or,concat,
                       length,any,reverse,
                       elem,all)
sum :: Num a => [a] -> a
sum = foldl (+) 0

prod :: Num a => [a] -> a
prod = foldl (*) 1

length :: [a] -> Int
length = foldl (\res _ -> res + 1) 0

elem :: Eq a -> [a] -> Bool
elem x = foldl (\res y -> res || x == y) False

and = undefined
or  = undefined

any = undefined
all = undefined

last = undefined

concat = undefined

reverse = undefined

-- euler5 = undefined
