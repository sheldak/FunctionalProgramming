newtype Box a = MkBox a deriving Show

instance Functor Box where
    fmap f (MkBox x) = MkBox (f x)

instance Applicative Box where
    pure = MkBox
    (MkBox f) <*> w = fmap f w

newtype MyTriple a = MyTriple (a, a, a) deriving Show

instance Functor MyTriple where
    fmap f (MyTriple (x, y, z)) = MyTriple (f x, f y, f z)

instance Applicative MyTriple where
    pure x = MyTriple (x, x, x)
    MyTriple (f, g, h) <*> MyTriple (x, y, z) = MyTriple (f x, g y, h z)

fun :: IO ()
fun = putStrLn "Podaj Imię "
    >> getLine
    >>= \n -> let line = read n :: String in
        putStrLn $ "Witaj: " ++ n

-- instance Applicative ((->) r) where
--     pure x = (\_ -> x)
--     f <*> g = \x -> f x (g x)

(+.) :: (Num x) => ((->) x ((->) x x))
a +. b = a + b