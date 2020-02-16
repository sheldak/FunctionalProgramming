import Test.QuickCheck

prop_plusAssociativeInt :: Int -> Int -> Int -> Bool
prop_plusAssociativeInt x y z = x + (y + z) == (x + y) + z

main :: IO ()
main = do
    putStrLn "\n*** Testing prop_plusAssociativeInt... ***"
    quickCheck (withMaxSuccess 1000 prop_plusAssociativeInt)
