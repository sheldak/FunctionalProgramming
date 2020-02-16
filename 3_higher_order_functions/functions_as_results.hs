sqr x = x^2

funcFactory n = case n of
    1 -> id
    2 -> sqr
    3 -> (^3)
    4 -> \x -> x^4
    5 -> intFunc
    _ -> const n
    where
        intFunc x = x^5

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n = case n of 
    0 -> \x -> 1
    1 -> \x -> x + 1
    _ -> \x -> ((x^n) / fromIntegral (factorial n)) + expApproxUpTo (n-1) x
    where 
        factorial 0 = 1
        factorial k = k * factorial (k-1)

dfr :: (Double -> Double) -> Double -> (Double -> Double)
dfr f h = \x -> (f (x+h) - f (x)) / h

dfc :: (Double -> Double) -> Double -> (Double -> Double)
dfc f h = \x -> (f (x+h) - f (x-h)) / (2*h)

d2f :: (Double -> Double) -> Double -> (Double -> Double)
d2f f h = \x -> (dfc f h (x+h) - dfc f h x) / h