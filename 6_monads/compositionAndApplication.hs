(<$<) :: (a -> b) -> a -> b
(<$<) = ($)

(>$>) :: a -> (a -> b) -> b
x >$> f = f x
infixl 0 >$>

(<.<) :: (b -> c) -> (a -> b) -> (a -> c)
(<.<) = (.)

(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
f >.> g = g . f
infixl 9 >.>

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

extractMaybe :: Maybe a -> a
extractMaybe Nothing = error "Nothing inside!"
extractMaybe (Just x) = x

insertMaybe :: a -> Maybe a
insertMaybe = Just

-- (>^$>) = extract (^) and apply ($)
(>^$>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >^$> _ = Nothing
(Just x) >^$> f = f x
infixl 1 >^$>

f1 :: (Ord a, Num a) => a -> Maybe a
f1 x = if x > 0 then Just (x + 1) else Nothing

f2 :: (Eq a, Num a) => a -> Maybe a
f2 x = if x /= 0 then Just (10 * x) else Nothing

-- Kleisli composition
(>.>>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
-- f >.>> g = \x -> (f x) >^$> g
f >.>> g = \x -> joinMaybe (g <$> (f x))

joinMaybe :: Maybe (Maybe a) -> (Maybe a)
joinMaybe (Just Nothing) = Nothing
joinMaybe (Just (Just x)) = Just x


extractFst :: (a, String) -> a
extractFst (x, _) = x

extractSnd :: (a, String) -> String
extractSnd (_, s) = s

insert :: a -> String -> (a, String)
insert x s = (x, s)

(>^$^>) :: (a, String) -> (a -> (b, String)) -> (b, String)
(a, s) >^$^> f = f a

(>>.>>) :: (a -> (b, String)) -> (b -> (c, String)) -> (a -> (c, String))
f >>.>> g = \x -> (f x) >^$^> g 