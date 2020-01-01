isPalindrome :: [Char] -> Bool
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:xs) = (x == last xs) && (isPalindrome (init xs))

getElemAtIdx :: Int -> [a] -> a
getElemAtIdx x y = head (drop x y)

toUpper :: Char -> Char
toUpper a = toEnum (fromEnum a - 32)

capitalize :: [Char] -> [Char]
capitalize (w:ws) = [toUpper w] ++ ws