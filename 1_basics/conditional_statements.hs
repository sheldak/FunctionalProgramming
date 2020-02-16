sgn :: Int -> Int
sgn n = if n < 0
        then -1
        else if n == 0
            then 0
            else 1

absInt :: Int -> Int
absInt n = if n < 0
        then -n
        else n

min2Int :: (Int, Int) -> Int
min2Int (x, y) = if x > y
                    then y
                    else x

min3Int :: (Int, Int, Int) -> Int
min3Int (x, y, z) = min2Int(min2Int(x, y), z)

toUpper :: Char -> Char
toUpper a = toEnum (fromEnum a - 32) :: Char

toLower :: Char -> Char
toLower a = toEnum (fromEnum a + 32) :: Char

isDigit :: Char -> Bool
isDigit a = 48 <= fromEnum a && fromEnum a <= 57

charToNum :: Char -> Int
charToNum a = fromEnum a - 48

romanDigit :: Char -> String
romanDigit a | (fromEnum a - 48 >= 1 && fromEnum a - 48 <= 9) = [a]
