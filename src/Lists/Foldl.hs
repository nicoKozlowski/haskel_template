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

elem :: Eq a => a -> [a] -> Bool
elem x xs = foldl (\res y -> res || x == y) False xs

and :: [Bool] -> Bool
and = foldl (&&) False

or :: [Bool] -> Bool
or  = foldl (||) True

any :: (a -> Bool) -> [a] -> Bool
any f = foldl (\res x -> res || f x ) False

all :: (a -> Bool) -> [a] -> Bool
all f = foldl (\res x -> res && f x) True

last :: [a] -> a
last xs = foldl (\_ x -> x) (head xs) xs

concat :: [[a]] -> [a]
concat = foldl (++) []

reverse :: [a] -> [a]
reverse xs = foldl (\res x -> [x] ++ res) [] xs

-- euler5 = undefined
