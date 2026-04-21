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

last =

init = undefined

elem = undefined

and = undefined
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
