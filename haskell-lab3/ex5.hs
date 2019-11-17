import Data.List

sortDesc :: Ord a => [a] -> [a]
sortDesc xs = (reverse . sort) xs

sortDesc' :: Ord a => [a] -> [a]
sortDesc' = reverse . sort

composeFunList :: [a->a] -> (a->a)
composeFunList [] = id
composeFunList (f:fs) = f . composeFunList fs

