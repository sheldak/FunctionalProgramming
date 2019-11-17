dfc :: (Double -> Double) -> Double -> (Double -> Double)
dfc f h = \x -> (f (x+h) - f (x-h)) / (2*h)

d2f :: (Double -> Double) -> Double -> (Double -> Double)
d2f f h = \x -> (dfc f h (x+h) - dfc f h x) / h

funcList :: [ Double -> Double ]
funcList = [ \x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x ]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x [] = []
evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs

displEqs :: (Double -> Double, Double -> Double)
displEqs = (\t -> 4 * t^2 + 2 * t, \t -> 3 * t^2)

calcVel :: [Double -> Double] -> Double -> [Double]
calcVel [] _ = []
calcVel (f:fs) x = dfc f 0.1 x : calcVel fs x

calcAcc :: [Double -> Double] -> Double -> [Double]
calcAcc [] _ = []
calcAcc (f:fs) x = d2f f 0.1 x : calcAcc fs x