qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
    where
        leftPart xs = filter (<=x) xs
        rightPart xs = filter (>x) xs

mSort :: Ord a => [a] -> [a]
mSort [] = []
mSort (x:[]) = [x]
mSort xs = merge [] (mSort firstHalf) (mSort secondHalf)
        where 
            firstHalf = take ((length xs) `div` 2) xs
            secondHalf = drop ((length xs) `div` 2) xs
            merge :: Ord a => [a] -> [a] -> [a] -> [a]
            merge x [] [] = x
            merge x y [] = x ++ y
            merge x [] y = x ++ y
            merge x (y:ys) (z:zs) | y <= z = merge (x ++ [y]) ys (z:zs)
                                  | otherwise = merge (x ++ [z]) (y:ys) zs
    

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort xs = insert (iSort (init xs)) [last xs]
        where
            insert :: Ord a => [a] -> [a] -> [a]
            insert [] (y:[]) = [y]
            insert (x:xs) (y:[]) | x >= y    = [y] ++ (x:xs)
                                 | otherwise = [x] ++ (insert xs [y])

concat' :: [[a]] -> [a]
concat' c = [ x | y <- c, x <- y]

concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (c:cs) = c ++ concat'' cs

isSorted :: [Int] -> Bool 
isSorted [] = True
isSorted (x:[]) = True
isSorted (x:y:ys) | x <= y = True && isSorted (y:ys)
                  | otherwise = False

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' x [] = []
zip' [] x = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys

unzip' :: [(a, b)] -> ([a],[b])
unzip' z = ([ x | (x,_) <- z], [ y | (_,y) <- z])

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' (x:xs) (y:ys) (z:zs) = [(x,y,z)] ++ zip3' xs ys zs
zip3' _ _ _ = []

subList :: Eq a => [a] -> [a] -> Bool
subList x [] = False
subList x (y:ys) | x == (take (length x) (y:ys)) = True
                 | otherwise = False || subList x ys