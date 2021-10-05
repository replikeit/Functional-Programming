import Data.List

iota :: (Eq a, Num a) => a -> [a]
iota 0 = []
iota x = let p = x - 1 in p : iota p

iotaRHelper x 0 = []
iotaRHelper x r = let p = x - r in p : iotaRHelper x (pred r)

iotaR x = iotaRHelper x x

-- Список чисел
iotaF b x =
    if x == b
    then Nothing
    else Just(x, x + 1)

xUnIota n = unfoldr (iotaF n) 0

--Двоичное представление
binUnfoldF x =
    if x == 0
    then Nothing
    else Just(mod x 2, div x 2)

binUnfold n = unfoldr (binUnfoldF) n


-- Список простых делителей
getPrime n i = if mod n i == 0 
    then i 
    else getPrime n (i + 1)

primesUnfoldF x =
    let d = getPrime x 2 in 
    if x == 1
    then Nothing
    else Just(d, div x d)

primesUnfold n = unfoldr (primesUnfoldF) n

-- Фибоначчи 
fibUnfold :: [Integer]
fibUnfold = unfoldr (\(f,s) -> Just (f,(s,f + s))) (0,1)
