polarToCartesian :: Floating a => (a,a) -> (a,a)
polarToCartesian (r, phi) = (r * cos phi, r * sin phi)

type CartesianCoord' a = (a,a)
type PolarCoord' a = (a,a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r, phi) = (r * cos phi, r * sin phi)

newtype CartesianCoord'' a = MkCartesianCoord'' (a,a) deriving Show
newtype PolarCoord'' a = MkPolarCoord'' (a,a) deriving Show

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r, phi)) = MkCartesianCoord'' (r * cos phi, r * sin phi)

newtype CartesianCoord3d'' a = MkCartesianCoord3d'' (a,a,a) deriving Show
newtype CylindricalCoord'' a = MkCylindricalCoord'' (a,a,a) deriving Show


cylindricalToCartesian'' :: Floating a => CylindricalCoord'' a -> CartesianCoord3d'' a
cylindricalToCartesian'' (MkCylindricalCoord'' (r, phi, h)) = MkCartesianCoord3d'' (r * cos phi, r * sin phi, h)

personInfoToString :: (String, String, String) -> String
personInfoToString (nm, snm, addr) = 
    "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String

personInfoToString' :: PersonInfoToStringType'
personInfoToString' (nm, snm, addr) = "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr