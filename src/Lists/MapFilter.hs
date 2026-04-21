module Lists.MapFilter where

xs = map (\x->x^2)(filter (\x->mod x 2==0) [1..9])

sqrs  = map (\x -> x^2) [1..6]

euler1 = sum (filter (\x -> mod x 3 == 0 || mod x 5 == 0) [0..999])

finde = undefined
