module Functions.Functions where

-- "Get Programming with Haskell" S. 80
collatz 1 = 1
collatz n = if even n
            then collatz (n `div` 2)
            else collatz (n*3 + 1)

ggT :: Int -> Int -> Int
ggT a b = if b == 0 then a else ggT b (a `mod` b)

fact :: Int -> Int
fact n = if n == 0 then 1 else n * fact (n - 1)

binom :: Int -> Int -> Int
binom n k = else if k < 0 || k > n then 0
            if k == 0 || k == n then 1
            else binom (n - 1) (k - 1) + binom (n - 1) k

fib :: Int -> Int
fib n = if n == 0 then 0
        else if n == 1 then 1
        else fib (n - 1) + fib (n - 2)