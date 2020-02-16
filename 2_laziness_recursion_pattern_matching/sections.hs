fiveToPower_ :: Integer -> Integer
fiveToPower_ x = 5^x

_ToPower5 :: Num a => a -> a
_ToPower5 x = x^5

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 x = 5 - x

subtr5From_ :: Num a => a -> a
subtr5From_ x = x - 5

flip2 :: (a -> b -> c) -> (b -> a -> c)
flip2 f x y = f y x

power :: Integer -> Integer -> Integer
power x y = x^y

power3 :: Integer -> Integer -> Integer -> Integer
power3 x y z = x^(y^z)

flip3 :: (a -> b -> c -> d) -> (c -> a -> b -> d)
flip3 f z x y = f x y z
