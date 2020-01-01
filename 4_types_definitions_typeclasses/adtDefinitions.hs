-- module Stack
--     ( Stack
--     , empty
--     , isEmpty
--     , push
--     , top
--     , pop
--     ) where

-- empty :: Stack a
-- isEmpty :: Stack a -> Bool
-- push :: a -> Stack a -> Stack a
-- top :: Stack a -> a
-- pop :: Stack a -> (a, Stack a)

-- newtype Stack a = MkStack [a] deriving Show

-- empty = MkStack []
-- isEmpty (MkStack s) = null s
-- push x (MkStack s) = MkStack (x:s)
-- top (MkStack s) = head s
-- pop (MkStack (s:ss)) = (s,MkStack ss)


-- module Queue
--     ( Queue
--     , emptyQ
--     , isEmptyQ
--     , addQ
--     , remQ
--     ) where

-- emptyQ :: Queue a
-- isEmptyQ :: Queue a -> Bool
-- addQ :: a -> Queue a -> Queue a
-- remQ :: Queue a -> (a, Queue a)

-- newtype Queue a = MkQueue [a] deriving Show

-- emptyQ = MkQueue []
-- isEmptyQ (MkQueue q) = null q
-- addQ x (MkQueue q) = MkQueue (q ++ [x])
-- remQ (MkQueue (q:qs)) = (q, MkQueue qs) 


module Dequeue
    ( Dequeue
    , emptyDEQ
    , isEmptyDEQ
    , lengthDEQ
    , firstDEQ
    , lastDEQ
    , takeFrontDEQ
    , takeBackDEQ
    , pushFrontDEQ
    , popFrontDEQ
    , pushBackDEQ
    , popBackDEQ
    , fromListDEQ
    ) where

emptyDEQ :: Dequeue a
isEmptyDEQ :: Dequeue a -> Bool
lengthDEQ :: Dequeue a -> Int
firstDEQ :: Dequeue a -> Maybe a
lastDEQ :: Dequeue a -> Maybe a
takeFrontDEQ :: Int -> Dequeue a -> [a]
takeBackDEQ :: Int -> Dequeue a -> [a]
pushFrontDEQ :: Dequeue a -> a -> Dequeue a
popFrontDEQ :: Dequeue a -> Maybe (a, Dequeue a)
pushBackDEQ :: Dequeue a -> a -> Dequeue a
popBackDEQ :: Dequeue a -> Maybe (a, Dequeue a)
fromListDEQ :: [a] -> Dequeue a

newtype Dequeue a = MkDequeue [a] deriving Show

emptyDEQ = MkDequeue []
isEmptyDEQ (MkDequeue qs) = null qs
lengthDEQ (MkDequeue qs) = length qs

firstDEQ (MkDequeue []) = Nothing
firstDEQ (MkDequeue (q:qs)) = Just q

lastDEQ (MkDequeue []) = Nothing
lastDEQ (MkDequeue qs) = Just (last qs)

takeFrontDEQ n (MkDequeue (q:qs)) | n == 0    = []
                                  | otherwise = [q] ++ (takeFrontDEQ (n-1) (MkDequeue qs))

takeBackDEQ n (MkDequeue qss) = takeLastN (length qss - n) qss
                            where takeLastN num (q:qs) | num > 0   = takeLastN (num - 1) qs
                                                       | null qs   = [q]
                                                       | otherwise = (takeLastN num qs) ++ [q] 

pushFrontDEQ (MkDequeue qs) x = MkDequeue (x:qs)

popFrontDEQ (MkDequeue [])   = Nothing
popFrontDEQ (MkDequeue (q:qs)) = Just (q, MkDequeue qs)

pushBackDEQ (MkDequeue qs) x = MkDequeue (qs ++ [x])

popBackDEQ (MkDequeue []) = Nothing
popBackDEQ (MkDequeue qs) = Just (last qs, MkDequeue (init qs))

fromListDEQ xs = MkDequeue xs