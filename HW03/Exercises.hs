import Data.Char
import Data.List

-- EXERCISE 1 ==================================================

-- 1.1.
-- - Define a recursive function to compute the product of a list of elements.
--   product' :: Num a => [a] -> a

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs


-- 1.2.
-- - Define a recursive function 'headsOf' that takes a list of lists and
--   returns a list of their heads.
--   headsOf :: [[a]] -> [a]
--   headsOf [[1,2,3],[4,5],[6]] => [1,4,6]

headsOf :: [[a]] -> [a]
headsOf [] = []   -- ne valja
headsOf (xs:xss) = head xs : headsOf xss


-- EXERCISE 2 ==================================================

-- 2.1.
-- - Define a recursive function 'modMult n m xs' that multiplies each element of
--   a list 'xs' with 'n' modulo 'm'.

modMult :: Int -> Int -> [Int] -> [Int]
modMult _ _ [] = []
modMult n m (x:xs) = (modul*x) : modMult n m xs
  where modul = n `mod` m

-- 2.2.
-- - Define a function 'addPredecessor' that adds to each element of a list the
--   value of the preceding element. The first element gets no value added.
--   addPredecessor :: Num a => [a] -> [a]
--   addPredecessor [3,2,1] => [3,5,3]

addPredecessor :: Num a => [a] -> [a]
addPredecessor xs = step 0 xs
    where
        step _ []     = []
        step p (x:xs) = x + p : step x xs

-- EXERCISE 3 ==================================================

-- 3.1.
-- - Define 'equalTriplets' that filters from a list of triplets (x,y,z) all
--   triplets for which x==y==z.
--   equalTriplets [(1,2,3),(2,2,2),(4,5,6)] => [(2,2,2)]

equalTriplets :: Eq a => [(a,a,a)] -> [(a,a,a)]
equalTriplets [] = []
equalTriplets ((x,y,z):xs)
    | x == y && y == z  = (x,y,z):equalTriplets xs
    | otherwise         = equalTriplets xs


-- 3.2.
-- - Define your own version of the replicate function:
--   replicate' :: Int -> a -> [a]

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x:replicate' (n-1) x

-- EXERCISE 4 ==================================================

-- 4.1.
-- - Define your own recursive version of the drop function:
--   drop' :: Int -> [a] -> [a].
-- - Define drop'' (a wrapper function) so that for n < 0 the function drops
--   the elements from the end of the list. You can use 'reverse'.

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) xs

drop'' :: Int -> [a] -> [a]
drop'' n xs
    | n < 0     = reverse $ drop''' (-n) $ reverse xs
    | otherwise = drop' n xs
    where
        drop''' 0 xs = xs
        drop''' _ [] = []
        drop''' n (x:xs) = drop' (n-1) xs


-- 4.2.
-- - Define a recursive function 'takeFromTo n1 n2 xs'.
--   takeFromTo :: Int -> Int -> [a] -> [a]

takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo _ _  []      = []
takeFromTo 0 0  xs      = []
takeFromTo 0 n2 (x:xs)  = x:takeFromTo 0 (n2-1) xs
takeFromTo n1 n2 (x:xs)
    | n2 > n1   = takeFromTo (n1-1) (n2-1) xs
    | otherwise = takeFromTo (n2-1) (n1-1) xs -- for cases like "takeFromTo 4 2"


-- EXERCISE 5 ===================================================

-- 5.1.
-- - Define a recursive function 'eachThird' that retains every third element
--   in a list.
--   eachThird :: [a] -> [a]
--   eachThird "zagreb" => "gb"

eachThird :: [a] -> [a]
eachThird (x:y:z:xs) = z:eachThird xs
eachThird _          = []


-- 5.2.
-- - Define a recursive function 'crossZip' that zips two lists in a "crossing"
--   manner:
--   crossZip [1,2,3,4,5] [4,5,6,7,8] => [(1,5),(2,4),(3,7),(4,6)]

crossZip :: [a] -> [b] -> [(a,b)]
crossZip []       _         = []
crossZip _        []        = []
crossZip [a]      _         = []
crossZip _        [x]    = []
crossZip (a:b:bs) (x:y:ys)  = (a,y):(b,x):crossZip bs ys


-- EXERCISE 6 ==================================================

-- 6.1.
-- - Write an accumulator-style recursive definition of
--   length' :: [a] -> Int

length' :: [a] -> Int
length' xs = length'' xs 0
  where
    length'' [] n = n
    length'' (x:xs) n = length'' xs (n+1)

-- 6.2
-- - Write an accumulator-style recursive definition of
--     maxUnzip :: [(Int, Int)] -> (Int, Int)
--   that returns the maximum element at the first position and the maximum
--   element at the second position in a pair, i.e., it's equivalent to:
--     maxUnzip zs = (maximum xs, maximum ys)
--       where (xs,ys) = unzip zs
--   If the list is empty, return an "empty list" error.
-- - Now write a standard recursive definition (without an accumulator).

maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip [] = error "empty list"
maxUnzip xs = maxUnzip' xs (minBound, minBound)
  where
    maxUnzip' [] (a,b) = (a,b)
    maxUnzip' ((x,y):xs) (a,b)
      | x < a && y < b = maxUnzip' xs (a,b)
      | x > a && y < b = maxUnzip' xs (x,b)
      | x < a && y > b = maxUnzip' xs (a,y)
      | otherwise      = maxUnzip' xs (x,y)

maxUnzip' :: [(Int, Int)] -> (Int ,Int)
maxUnzip' [] = error "empty list"
maxUnzip' [(x,y)] = (x,y)
maxUnzip' ((x,y):(a,b):xs)
  | x < a && y < b = maxUnzip' $ (a,b):xs
  | x < a && y > b = maxUnzip' $ (a,y):xs
  | x > a && y < b = maxUnzip' $ (x,b):xs
  | otherwise      = maxUnzip' $ (x,y):xs
