absInt :: Int -> Int
absInt n | n >= 0 = n
         | otherwise = -n

min3Int :: (Int, Int, Int) -> Int
min3Int (a, b, c) | a <= b && a <= c = a
                  | b <= a && b <= c = b
                  | otherwise = c   