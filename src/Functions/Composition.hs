module Functions.Composition where
import Functions.Operators
import Functions.Functions
import Prelude hiding (even, odd, not)

even :: Int -> Bool
even = not . odd

evenFib :: Int -> Bool
evenFib = even . fib