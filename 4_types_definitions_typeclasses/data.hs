type X = Int
type Y = Int

data CartInt2DVec = MkCartInt2DVec X Y

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

data Cart2DVec' a = MkCart2DVec' a a deriving Show

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

instance (Show a) => Show (Cart2DVec'' a) where
    show MkCart2DVec'' {x=x,  y=y} = "MkCart2DVec''" ++ show x ++ " " ++ show y

data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x

-- data ThreeColors = Blue |
--                    White |
--                    Red

-- type ActorName = String

-- leadingActor :: ThreeColors -> ActorName
-- leadingActor Blue  = "Juliette Binoche"
-- leadingActor White = "Zbigniew Zamachowski"
-- leadingActor Red   = "Irene Jacob"

data Cart3DVec = MkCart3DVec Int Int Int

xCoord3D :: Cart3DVec -> Int
xCoord3D (MkCart3DVec x _ _) = x

yCoord3D :: Cart3DVec -> Int
yCoord3D (MkCart3DVec _ y _) = y

zCoord3D :: Cart3DVec -> Int
zCoord3D (MkCart3DVec _ _ z) = z

--data Cart3DVec' a = MkCart3DVec' {x::a, y::a, z::a}

data Polar2DVec a = MkPolar2DVec a a deriving Show

data Polar2DVec' a = MkPolar2DVec' {r::a, phi::a} deriving Show

polarToCartesian :: Floating a => Polar2DVec a -> Cart2DVec' a
polarToCartesian (MkPolar2DVec r phi) = MkCart2DVec' (r * cos phi) (r * sin phi)

polarToCartesian' :: Floating a => Polar2DVec' a -> Cart2DVec'' a
polarToCartesian' (MkPolar2DVec' r phi) = MkCart2DVec'' (r * cos phi) (r * sin phi)

data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle x) = pi * x * x
area (Rectangle x y) = x * y

data Tree a = EmptyT |
              Node a (Tree a) (Tree a)
              deriving Show

rootValue :: Tree a -> a
rootValue EmptyT     = error "rootValue: the empty tree has no root!"
rootValue (Node x xa xb) = x

data TrafficLights = Red |
                     Yellow |
                     Green

actionFor :: TrafficLights -> String
actionFor Red = "stop!"
actionFor Yellow = "be ready!"
actionFor Green = "go!"