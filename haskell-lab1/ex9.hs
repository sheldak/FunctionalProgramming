roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ((-b - d) / e, (-b + d) / e)
    where {d = sqrt (b * b - 4 * a * c);
            e = 2 * a} -- roots of quadratic equation
{- now there is some vector stuff
and quick way to calculate area of triangle! -}
unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (x, y) = (x / z, y / z)
    where z = sqrt(x*x + y*y)

unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (x, y, z) = (x / a, y / a, z / a)
    where a = sqrt(x*x + y*y + z*z)

heronArea :: (Double, Double, Double) -> Double
heronArea (a, b, c) = sqrt(p*(p-a)*(p-b)*(p-c))
    where p = (a+b+c)/2