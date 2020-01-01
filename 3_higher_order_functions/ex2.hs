sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' [] = 0
sumSqr'(x:xs) = x^2 + sumSqr' xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = f 0
sumWith f (x:xs) = f x + sumWith f xs

sum'' = sumWith id
sumSqr'' = sumWith (^2)
sumCube'' = sumWith (^3)
sumAbs'' = sumWith abs

listLength'' a = (sumWith (\x -> 1) a) - 1

prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs

prodWith :: Num a => (a->a) -> [a] -> a
prodWith f [] = f 1
prodWith f (x:xs) = f x * prodWith f xs

prod'' = prodWith id
prodSqr'' = prodWith (^2)
prodCube'' = prodWith (^3)
prodAbs'' = prodWith abs

doWith :: Num a => (a -> a) -> (a -> a -> a) -> a -> [a] -> a
doWith f g z [] = f z
doWith f g z (x:xs) = g (f x) (doWith f g z xs)

sumSqrt = sumWith sqrt
prodSqrt = prodWith sqrt