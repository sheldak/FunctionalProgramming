sqr :: Double -> Double
sqr x = x * x

vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x, y, z) = sqrt(sqr x + sqr y + sqr z)

swap :: (Int, Char) -> (Char, Int)
swap (x, s) = (s, x)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x, y, z) = x == y && y == z

heronArea :: (Double, Double, Double) -> Double
heronArea (a, b, c) = sqrt((a + b + c)*(a + b - c)*(a - b + c)*(-a + b + c)) / 4