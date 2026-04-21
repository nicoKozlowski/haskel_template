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

and
or = undefined

any = undefined

all = undefined

maximum = undefined

minimum = undefined

append = undefined

concat = undefined

take = undefined

drop = undefined

takeWhile = undefined

dropWhile = undefined
     
map = undefined

filter = undefined
  
reverse = undefined

partition = undefined
