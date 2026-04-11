module Functions.Composition where
import Functions.Operators
import Functions.Functions
import Prelude hiding (even, odd, not)

even :: Int -> Bool
even n = not(odd(n))

evenFib :: Int -> Bool
evenFib n = even(fib(n))