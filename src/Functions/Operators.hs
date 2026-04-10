module Functions.Operators where
import Prelude hiding (max,not,odd,and,or,nand,nor)

add :: Int -> Int -> Int
add x y  = x + y

addRec :: Int -> Int -> Int
addRec x 0 = x
addRec x y = addRec (x+1) (y-1)

succ :: Int -> Int
succ x = x + 1

max :: Int -> Int -> Int
max a b = if a > b then a else b

not :: Bool -> Bool
not True = False
not False = True

and :: Bool -> Bool -> Bool
and x y = x && y

or :: Bool -> Bool -> Bool
or x y = x || y

nand :: Bool -> Bool -> Bool
nand x y = not (x && y)

nor :: Bool -> Bool -> Bool
nor x y = not (x || y)

odd :: Int -> Bool
odd x = x `mod` 2 /= 0