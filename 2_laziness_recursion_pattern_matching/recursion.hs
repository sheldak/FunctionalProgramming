{-# LANGUAGE BangPatterns #-}
fib :: (Num a, Eq a) => a -> a
fib n 
    | n == 0 || n == 1 = n
    | otherwise = fib (n - 2) + fib (n - 1)

fib2 :: Int -> Int
fib2 n = fibs !! n
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]

sum' :: Num a => [a] -> a -- stack overflow for size >= 9124385
sum' [] = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool
elem' n [] = False
elem' n (x:xs) = (x == n) || elem' n xs

doubleAll :: Num t => [t] -> [t]
doubleAll [] = []
doubleAll (x:xs) = [2*x] ++ doubleAll xs

squareAll :: Num t => [t] -> [t]
squareAll [] = []
squareAll (x:xs) = [x^2] ++ squareAll xs

selectEven :: Integral t => [t] -> [t]
selectEven [] = []
selectEven (x:xs) | x `mod` 2 == 0 = [x] ++ selectEven xs
                  | otherwise = selectEven xs

mean' :: [Double] -> Double
mean' x = sum' x / fromIntegral (length' x)

geoMean' :: [Double] -> Double
geoMean' x = prod' x ** (1 / fromIntegral (length' x))

twoMeans :: [Double] -> (Double, Double)
twoMeans x = (mean' x, geoMean' x)

sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
    where loop acc [] = acc
          loop acc (x:xs) = loop (x + acc) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
    where loop acc [] = acc 
          loop acc (x:xs) = loop (x + acc) xs

prod'2 :: Num a => [a] -> a
prod'2 = loop 1
    where loop acc [] = acc
          loop acc (x:xs) = loop (x * acc) xs

sum'4 :: Num a => [a] -> a
sum'4 = loop 0
    where loop !acc [] = acc
          loop !acc (x:xs) = loop (x + acc) xs