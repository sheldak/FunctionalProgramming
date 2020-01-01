{-# LANGUAGE DeriveFunctor #-}

newtype Box a = MkBox a deriving (Show, Functor)

-- instance Functor Box where
--     fmap f (MkBox x) = MkBox (f x)

data MyList a = EmptyList | Cons a (MyList a) deriving (Show, Functor)

-- instance Functor MyList where
--     fmap _ EmptyList    = EmptyList
--     fmap f (Cons x mxs) = Cons (f x) (fmap f mxs)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Functor)

-- instance Functor Tree where
--     fmap _ Empty         = Empty
--     fmap f (Node x xs xss) = Node (f x) (fmap f xs) (fmap f xss)