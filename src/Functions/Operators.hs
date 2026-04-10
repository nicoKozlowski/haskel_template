module Functions.Operators where
import Prelude hiding (max,not,odd)

add :: Int -> Int -> Int
add x y  = x + y

addRec :: Int -> Int -> Int
addRec x 0 = x
addRec x y = addRec (x+1) (y-1)

succ :: Int -> Int
succ x = x + 1

max :: Int -> Int -> Int
max a b = a > b ? a : b

not :: Boolean -> Boolean
not x = x -> !x

and :: Boolean -> Boolean -> Boolean
and x y = x -> y -> x && y

or :: Boolean -> Boolean -> Boolean
or x y = x -> y -> x || y

nand :: Boolean -> Boolean -> Boolean
nand x y = x -> y -> !(x && y)

nor :: Boolean -> Boolean -> Boolean
nor x y = x -> y -> !(x || y)

odd :: Integer -> Boolean
odd x = x -> x % 2 != 0