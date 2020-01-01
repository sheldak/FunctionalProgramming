newtype MyInt = MkMyInt Int

instance Eq MyInt where
    (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2

instance Ord MyInt where
    (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

instance Num MyInt where
    (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
    (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
    (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
    negate (MkMyInt i)            = MkMyInt (negate i)
    abs (MkMyInt i)               = MkMyInt (abs i)
    signum (MkMyInt i)            = MkMyInt (signum i)
    fromInteger int               = MkMyInt (fromInteger int)

instance Show MyInt where
    show (MkMyInt i) = "MkMyInt " ++ show i


data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a) deriving Show

instance Eq a => Eq (BinTree a) where
    (==) EmptyBT EmptyBT = True
    (==) EmptyBT (NodeBT _ _ _) = False
    (==) (NodeBT _ _ _) EmptyBT = False
    (==) (NodeBT a1 lt1 rt1) (NodeBT a2 lt2 rt2) = a1 == a2 && lt1 == lt2 && rt1 == rt2


data Cart3DVec = MkCart3DVec Int Int Int deriving (Show, Ord)

instance Eq Cart3DVec where
    (==) (MkCart3DVec x1 y1 z1) (MkCart3DVec x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2 

instance Num Cart3DVec where
    (+) (MkCart3DVec x1 y1 z1) (MkCart3DVec x2 y2 z2) = MkCart3DVec (x1 + x2) (y1 + y2) (z1 + z2)
    (*) (MkCart3DVec x1 y1 z1) (MkCart3DVec x2 y2 z2) = MkCart3DVec (y1 * z2 - y2 * z1) (z1 * x2 - z2 * x1) (x1 * y2 - x2 * y1) -- cross product
    negate (MkCart3DVec x y z) = MkCart3DVec (-x) (-y) (-z)
    abs (MkCart3DVec x y z) = MkCart3DVec (abs x) (abs y) (abs z)
    signum (MkCart3DVec x y z) = MkCart3DVec (signum x) (signum y) (signum z)
    fromInteger int = MkCart3DVec (fromInteger int) (fromInteger int) (fromInteger int)


data Fraction a = Fraction {num::a, denom::a} -- numerator, denominator

instance Show a => Show (Fraction a) where
    show (Fraction num denom) = show num ++ "/" ++ show denom

gcd' :: Integral a => a -> a -> a  -- greatest common divisor
gcd' x y | modulo == 0 = min x y
         | otherwise = gcd (min x y) modulo
         where modulo = (max x y) `mod` (min x y)

instance Integral a => Eq (Fraction a) where
    (==) (Fraction num1 denom1) (Fraction num2 denom2) = (num1 == num2 && (denom1 == denom2 || num1 == 0)) || multiple
                                                         where multiple = num1 /= 0 && num2 /= 0 && 
                                                                          num1 `div` gcd1 == num2 `div` gcd2 && denom1 `div` gcd1 == denom2 `div` gcd2 
                                                               gcd1 = gcd' num1 denom1
                                                               gcd2 = gcd' num2 denom2


newtype MyList a = MyList [a]

instance Eq a => Eq (MyList a) where
    (==) (MyList []) (MyList []) = True
    (==) (MyList []) (MyList _) = False
    (==) (MyList _) (MyList []) = False
    (==) (MyList (x:xs)) (MyList (y:ys)) = x == y && (MyList xs) == (MyList ys)
