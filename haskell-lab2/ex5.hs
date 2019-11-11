isPrime :: Integral t => t -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []

primesIn :: Int -> Int
primesIn n = length [i | i <- [2..n], isPrime i]

primes :: [Int]
primes = eratoSieve [2..]
    where
        eratoSieve :: [Int] -> [Int]
        eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]


isPrime' :: Int -> Bool
isPrime' n = inPrimes n primes
    where
        inPrimes :: Int -> [Int] -> Bool
        inPrimes n (x:xs) = if x > n then False else
                                if x == n then True else inPrimes n xs

primesIn' :: Int -> Int
primesIn' n = inPrimes n primes
    where
        inPrimes :: Int -> [Int] -> Int
        inPrimes n (x:xs) = if x > n then 0 else 1 + inPrimes n xs

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:[]) = True
allEqual (x:y:ys) = (x == y) && (allEqual (y:ys))