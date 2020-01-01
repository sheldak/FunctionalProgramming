isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = foldr (&&) True (zipWith (<=) xs (tail xs))

everySecond :: [t] -> [t]
everySecond xs = map fst $ filter (odd . snd) $ zip xs [1..]

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' _ _ [] = []
zip3' _ [] _ = []
zip3' [] _ _ = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : (zip3' xs ys zs)

trd' :: (a, a, a) -> a
trd' (x, y, z) = z

snd' :: (a, a, a) -> a
snd' (x, y, z) = y

unzip3' :: [(a, b, c)] -> ([a], [b], [c]) -- nie dziala
unzip3' xs = (map fst xs, map snd' xs, map trd' xs)