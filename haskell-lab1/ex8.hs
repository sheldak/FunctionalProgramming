not' :: Bool -> Bool
not' b = case b of
          True -> False
          False -> True

absInt n = 
    case (n >= 0) of
        True -> n
        _    -> -n

isItTheAnswer :: String -> Bool
isItTheAnswer s =
    case (s == "Love") of
        True -> True
        _    -> False

or' :: (Bool, Bool) -> Bool
or' (x, y) = 
    case (x || y) of
        True -> True
        _    -> False

and' :: (Bool, Bool) -> Bool
and' (x, y) = 
     case (x && y) of
        True -> True
        _    -> False


nand' :: (Bool, Bool) -> Bool
nand' (x, y) = 
    case (not' (x && y)) of
        True -> True
        _    -> False


xor' :: (Bool, Bool) -> Bool
xor' (x, y) = 
    case (x == y) of
        True -> False
        _    -> True