data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving Show

instance Foldable Tree where
    foldMap f Empty        = mempty
    foldMap f (Leaf x)     = f x
    foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r

instance Functor Tree where
    fmap f Empty        = Empty
    fmap f (Leaf x)     = Leaf $ f x
    fmap f (Node l x r) = Node (fmap f l) 
                               (f x)
                               (fmap f r)

instance Traversable Tree where
    traverse f Empty        = pure Empty
    traverse f (Leaf x)     = Leaf <$> f x
    traverse f (Node l x r) = Node <$> traverse f l
                                   <*> f x
                                   <*> traverse f r
