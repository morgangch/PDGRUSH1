{-
-- EPITECH PROJECT, 2025
-- my
-- File description:
-- a lot of fonctions
-}

module My where

mySucc :: Int -> Int
mySucc a = a + 1

myIsNeg :: Int -> Bool
myIsNeg a = a < 0

myAbs :: Int -> Int
myAbs a | myIsNeg(a) = -a
    | otherwise = a

myMin :: Int -> Int -> Int
myMin a b |a > b = b
    | otherwise = a

myMax :: Int -> Int -> Int
myMax a b | a < b = b
    | otherwise = a

myTuple :: a -> b -> (a , b )
myTuple a b = (a, b)

myTruple :: a -> b -> c -> (a , b , c )
myTruple a b c = (a, b, c)

myFst :: (a , b ) -> a
myFst (a, b) = a

mySnd :: (a , b ) -> b
mySnd (a, b) = b

mySwap :: (a , b) -> (b , a )
mySwap (a, b) = myTuple b a

myHead :: [a] -> a
myHead [] = error "Empty list"
myHead (a : b) = a

myTail :: [a] -> [a]
myTail [] = error "Empty list"
myTail (a : b) = b

myLength :: [a] -> Int
myLength [] = 0
myLength list = 1 + myLength (myTail list)

myNth :: [a] -> Int -> a
myNth _ n | n < 0 = error "N to low"
myNth [] _ = error "Empty list or N too high"
myNth (a: _) 0 = a
myNth list n = myNth (myTail list) (n - 1)

myTake :: Int -> [a] -> [a]
myTake n _ | n <= 0 = []
myTake _ [] = []
myTake n (a: b) = a : myTake (n - 1) b

myDrop :: Int -> [a] -> [a]
myDrop n list | n <= 0 = list
myDrop _ [] = []
myDrop n list = myDrop (n - 1) (myTail list)

myAppend :: [a] -> [a] -> [a]
myAppend [] list2 = list2
myAppend (a: b) list2 = a : myAppend b list2

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a: as) = (myAppend (myReverse as) [a])

myInit :: [a] -> [a]
myInit [] = error "Empty list"
myInit list = (myTake ((myLength list) - 1) list)

myLast :: [a] -> a
myLast [] = error "Empty list"
myLast (a: []) = a
myLast (_: b) = myLast b

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (a: b) (c: d) = (myTuple a c) : myZip b d

myUnzip :: [(a , b)] -> ([a] , [b])
myUnzip [] = myTuple [] []
myUnzip ((a, b): c) =  myTuple (a : as) (b : bs)
    where
        (as, bs) = myUnzip c

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap fonction (a : b) = (fonction a) : myMap fonction b

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter fonction (a : b) | fonction a = a : myFilter fonction b
    | otherwise = myFilter fonction b

myFoldl :: (b->a->b) -> b -> [a] -> b
myFoldl _ val [] = val
myFoldl fonction val (a: b) = myFoldl fonction (fonction val a) b

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ val [] = val
myFoldr fonction val (a: b) = fonction a (myFoldr fonction val b)

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