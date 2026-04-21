module Lists.Comprehension where
import Prelude hiding (map,filter)


xs =  [x^2 | x <- [1..9], mod x 2==0 ]

sqrs = [x^2 | x <- [1..6], mod x 2 == 0]

euler1 = sum [x | x <- [0..999], mod x 3 == 0 || mod x 5 == 0]

prodsOdd =

cartProd = undefined

map = undefined

filter = undefined
