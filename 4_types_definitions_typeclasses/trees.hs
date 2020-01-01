data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a) deriving Show

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (NodeBT _ lt rt) = 1 + max (depthOfBT lt) (depthOfBT rt)

flattenBTPreorder :: BinTree  a -> [a]
flattenBTPreorder EmptyBT = []
flattenBTPreorder (NodeBT n lt rt) = [n] ++ flattenBTPreorder lt ++ flattenBTPreorder rt

flattenBTInorder :: BinTree  a -> [a]
flattenBTInorder EmptyBT = []
flattenBTInorder (NodeBT n lt rt) = flattenBTInorder lt ++ [n] ++ flattenBTInorder rt

flattenBTPostorder :: BinTree  a -> [a]
flattenBTPostorder EmptyBT = []
flattenBTPostorder (NodeBT n lt rt) = flattenBTPostorder lt ++ flattenBTPostorder rt ++ [n]

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT n lt rt) = NodeBT (f n) (mapBT f lt) (mapBT f rt )

insert :: Ord a => a -> BinTree a -> BinTree a
insert a EmptyBT = NodeBT a EmptyBT EmptyBT
insert a (NodeBT n lt rt) | a < n     = NodeBT n (insert a lt) rt
                          | otherwise = NodeBT n lt (insert a rt)

list2BST :: Ord a => [a] -> BinTree a
list2BST [] = EmptyBT
list2BST (x:xs) = insert x (list2BST xs)

occurs :: Eq a => a -> BinTree a -> Int
occurs x EmptyBT = 0
occurs x (NodeBT n lt rt) | n == x    = 1 + (occurs x lt) + (occurs x rt)
                          | otherwise = (occurs x lt) + (occurs x rt)

elemOf :: Eq a => a -> BinTree a -> Bool
elemOf x EmptyBT = False
elemOf x (NodeBT n lt rt) | n == x    = True
                          | otherwise = (elemOf x lt) || (elemOf x rt)

reflect :: BinTree a -> BinTree a
reflect EmptyBT = EmptyBT
reflect (NodeBT n lt rt) = NodeBT n (reflect rt) (reflect lt)

minElemOf :: Ord a => BinTree a -> a
minElemOf EmptyBT = error "empty tree"
minElemOf (NodeBT n EmptyBT _) = n
minElemOf (NodeBT n lt _) = minElemOf lt

maxElemOf :: Ord a => BinTree a -> a
maxElemOf EmptyBT = error "empty tree"
maxElemOf (NodeBT n _ EmptyBT) = n
maxElemOf (NodeBT n _ rt) = maxElemOf rt

instance Eq a => Eq (BinTree a) where
    (==) (EmptyBT) (EmptyBT) = True
    (==) (NodeBT a lt1 rt1) (NodeBT b lt2 rt2) = a == b && lt1 == lt2 && rt1 == rt2

foldBinTree :: (a -> b -> b -> b) -> b -> BinTree a -> b
foldBinTree f z EmptyBT = z
foldBinTree f z (NodeBT n lt rt) = (f n (foldBinTree f z lt) (foldBinTree f z rt))

mapBT' :: (a -> b) -> BinTree a -> BinTree b
mapBT' f = foldBinTree (\x y z -> NodeBT (f x) y z) EmptyBT 

zipBT :: BinTree a -> BinTree b -> BinTree (a, b)
zipBT EmptyBT _ = EmptyBT
zipBT _ EmptyBT = EmptyBT
zipBT (NodeBT n1 lt1 rt1) (NodeBT n2 lt2 rt2) = NodeBT (n1, n2) (zipBT lt1 lt2) (zipBT rt1 rt2)


data GTree a = Leaf a |
               GNode [GTree a]
               deriving Show

sumGTree :: Num a => GTree a -> a
sumGTree (Leaf x) = x
sumGTree (GNode xs) = foldr1 (+) (map sumGTree xs)

elemOfGTree :: Eq a => a -> GTree a -> Bool
elemOfGTree n (Leaf x) = n == x
elemOfGTree n (GNode xs) = foldr (||) False (map (elemOfGTree n) xs)

depthOfGTree :: GTree a -> Int
depthOfGTree (Leaf _) = 0
depthOfGTree (GNode xs) = foldr max 0 (map ((+1) . depthOfGTree) xs)

mapGTree :: (a -> b) -> GTree a -> GTree b
mapGTree f (Leaf x) = Leaf (f x)
mapGTree f (GNode xs) = GNode (map (mapGTree f) xs)

flattenGTree :: GTree a -> [a]
flattenGTree (Leaf x) = [x]
flattenGTree (GNode xs) = foldr (++) [] (map flattenGTree xs)

countGTreeLeaves :: GTree a -> Int
countGTreeLeaves (Leaf _) = 1
countGTreeLeaves (GNode xs) = sum (map countGTreeLeaves xs)

data Expr a = Lit a |
              Expr a :+: Expr a |
              Expr a :-: Expr a

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :-: e2) = eval e1 - eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (e1 :+: e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (e1 :-: e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
