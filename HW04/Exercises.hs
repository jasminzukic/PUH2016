import Data.Char
import Data.List hiding (map, filter)
import Prelude hiding (map, filter)

finishSentence :: String -> String
finishSentence = (++ ".")

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

-- EXERCISE 1 ======================================

-- Define the following functions using partial application of existing functions:

-- 1.1.
-- - Function 'takeThree' that takes the first three elements from a list.
-- - Function 'dropThree' that drops the first three elements from a list.
-- - Function 'hundredTimes' that takes one element and repeats it 100 times in a
--   list.

takeThree :: [a] -> [a]
takeThree = take 3

dropThree :: [a] -> [a]
dropThree = drop 3

-- hundredTimes = take 100 . repeat

hundredTimes :: a -> [a]
hundredTimes = replicate 100


-- 1.2.
-- - Define 'index' that indexes the elements in a list:
--     index "xyz" => [(0,'x'),(1,'y'),(2,'z')]
-- - Define index' in which the index comes at the second position in the pair.

index :: [a] -> [(Integer, a)]
index = zip [0..]

index' :: [a] -> [(a, Integer)]
index' = (`zip` [0..])


-- 1.3.
-- - Define 'divider n' that returns a string of length 'n' consisting of
--   characters '='.
--   divider :: Int -> [Char]
--   divider 3 => "==="

divider :: Int -> String
divider n = replicate n '='


-- === EXERCISE 2 ================================================================--

-- 2.1.
-- - Define 'applyOnLast f xs ys' that applies a binary function 'f' on the last
--   element of 'xs' and the last element of 'ys'.
--   applyOnLast (+) [1,2,3] [5,6] => 9
--   applyOnLast max [1,2] [3,4] => 4
-- - Using this function and the 'addThree' function, define
--   lastTwoPlus100 :: [Integer] -> [Integer] -> Integer
--   lastTwoPlus100 [1,2,3] [6,5] => 108--

addThree :: Num a => a -> a -> a -> a
addThree x y z = x + y + z

applyOnLast :: (a -> b -> c) -> [a] -> [b] -> c
applyOnLast f xs ys = f (last xs) (last ys)

lastTwoPlus100 :: [Integer] -> [Integer] -> Integer
lastTwoPlus100 xs ys = addThree 100 (applyOnLast min xs ys) (applyOnLast max xs ys)

-- 2.2.
-- - Define 'applyManyTimes n f x' that applies 'n' times function 'f' to argument
--   'x'. If n<=0, return 'x' unaltered.
--   applyManyTimes 5 (+2) 0 => 10
--   applyManyTimes 3 finishSentence "hm" => "hm..."
-- - Using this function, define 'applyTwice''

applyManyTimes :: Int -> (a -> a) -> a -> a
applyManyTimes n f x
    | n <= 0   = x
    | otherwise = applyManyTimes (n-1) f (f x)

applyTwice :: (a -> a) -> a -> a
applyTwice = applyManyTimes 2


-- === EXERCISE 3 ================================================================--

-- Write the following functions using 'map'.--

-- 3.1.
-- - listifylist :: [a] -> [[a]]
--   listifylist [1,2,3] => [[1],[2],[3]]--

listifyList :: [a] -> [[a]]
listifyList = map (:[])


-- 3.2.
-- - Define 'cutoff n xs', which cuts off all values from the lists 'xs' at
--   value 'n'.
--   cutoff :: Int -> [Int] -> [Int]
--   cutoff 100 [20,202,34,117] => [20,100,34,100]

cutoff :: Int -> [Int] -> [Int]
cutoff n = map (min n)


-- === EXERCISE 4 ================================================================--

-- Define the following functions using 'map' and 'filter':--

-- 4.1.
-- - Function 'sumEvenSquares' that adds the squares of all even numbers from a
--   list.
--   sumEvenSquares :: [Integer] -> Integer
--   sumEvenSquares [1,2,3,4] => 20

sumEvenSquares :: [Integer] -> Integer
sumEvenSquares xs = sum $ map (^2) $ filter even xs

-- 4.2.
-- - Function 'freq x xs' that counts how many times element 'x' occurs in list
--   'xs'.
--   freq :: Eq a => a -> [a] -> Int
--   freq 'k' "kikiriki" => 3

freq :: Eq a => a -> [a] -> Int
freq x xs = length $ filter (==x) xs

-- 4.3.
-- - Function 'freqFilter n' that filters all elements that occur at least 'n'
--   times in a list.
--   freqFilter :: Eq a => Int -> [a] -> [a]
--   freqFilter 4 "kikiriki" => "iiii"

freqFilter :: Eq a => Int -> [a] -> [a]
freqFilter n xs = freqFilter' n xs xs
    where
        freqFilter' n [] _ = []
        freqFilter' n (x:xs) ys
            | freq x ys < n = freqFilter' n xs ys
            | otherwise     = x:freqFilter' n xs ys


-- === EXERCISE 5 ================================================================--

-- Define the following functions using lambda expressions:--

-- 5.1.
-- - Define a function 'withinInterval n m xs' that filters from list 'xs' all
--   elements that fall within the [n,m] interval.--


withinInterval :: Ord a => a -> a -> [a] -> [a]
withinInterval n m = filter (\x -> x < m && x > n)


-- 5.2.
-- - Define 'sndColumn m' that returns the second column of matrix 'm',
--   represented as a list of lists.
--   sndColumn [[a]] -> [a]
--   sndColumn [[1,2,3],[4,5,6]] => [2,5]--

sndColumn :: [[a]] -> [a]
sndColumn = map (!! 1)

-- 5.3.
-- - Define 'canoinicalizePairs' that takes a list of pairs and returns a list of
--   pairs with the order of elements switched so that the first element of the
--   pair is smaller than the second one. If the elements are equal, the pair is
--   discarded.
--   canonicalizePairs :: Ord a => [(a, a)] -> [(a, a)]
--   canonicalizePairs [(4,1),(2,2),(1,5)] => [(1,4),(1,5)]

canoinicalizePairs :: Ord a => [(a, a)] -> [(a, a)]
canoinicalizePairs xs = map (\(x,y) -> if x<y then (x,y) else (y,x))
                            $ filter (uncurry (/=)) xs
