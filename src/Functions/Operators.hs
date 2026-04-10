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

not :: Bool -> Bool
not x = x -> !x

and :: Bool -> Bool -> Bool
and x y = x -> y -> x && y

or :: Bool -> Bool -> Bool
or x y = x -> y -> x || y

nand :: Bool -> Bool -> Bool
nand x y = x -> y -> !(x && y)

nor :: Bool -> Bool -> Bool
nor x y = x -> y -> !(x || y)

odd :: Int -> Bool
odd x = x -> x % 2 != 0