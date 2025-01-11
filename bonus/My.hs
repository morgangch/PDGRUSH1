{-
-- EPITECH PROJECT, 2025
-- my
-- File description:
-- a lot of fonctions
-}

module My (myQuickSort) where

myTuple :: a -> b -> (a , b )
myTuple a b = (a, b)

myAppend :: [a] -> [a] -> [a]
myAppend [] list2 = list2
myAppend (a: b) list2 = a : myAppend b list2

myPartition :: (a -> Bool) -> [a] -> ([a] , [a])
myPartition _ [] = myTuple [] []
myPartition fonction (a: b)
    | fonction a = myTuple (a: y) n
    | otherwise = myTuple y (a: n)
    where
        (y, n) = myPartition fonction b

myQuickSort :: (a -> a -> Bool) -> [a] -> [a]
myQuickSort _ [] = []
myQuickSort fonction (val:next) = myAppend 
    (myAppend (myQuickSort fonction lt) [val]) 
    (myQuickSort fonction gt)
  where
    (lt, gt) = myPartition (\fval -> fonction fval val) next
