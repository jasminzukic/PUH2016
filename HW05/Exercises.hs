import Prelude hiding (reverse)
import Data.Char
import Data.List hiding (reverse)
import Data.Tuple

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


-- EXERCISE 1 =====================================================

-- Define the following functions using composition and pointfree style (you may
-- of course use local definitions):

-- 1.1.
-- - Define 'sumEven' that adds up elements occurring at even (incl. zero)
--   positions in a list.
--   sumEven :: [Integer] -> Integer
--   sumEven [1..10] => 25

sumEven :: [Integer] -> Integer
sumEven = sum . map snd . filter (even . fst) . zip [0..]


-- 1.2.
-- - Define 'filterWords ws s' that removes from string 's' all words contained
--   in the list 'ws'.
--   filterWords :: [String] -> String -> String


filterWords :: [String] -> String -> String
filterWords ws = unwords . filter (`notElem` ws) . words

-- 1.3.
-- - Define 'initials3 d p s' that takes a string 's' and turns it into a string
--   of initials. The function is similar to 'initials2' but additionally delimits
--   the initials with string 'd' and discards the initials of words that don't
--   satisfy the predicate 'p'.
--   initials3 :: String -> (String -> Bool) -> String -> String
--   initials3 "." (/="that") "a company that makes everything" => "A.C.M.E."
-- - Use this function to define the 'initials' function.

initials3 :: String -> (String -> Bool) -> String -> String
initials3 d p s = concatMap ((:d) . toUpper . head) $ filter p $ words s


-- EXERCISE 2 =====================================================

-- 2.1.
-- - Define 'maxDiff xs' that returns the maximum difference between consecutive
--   elements in the list 'xs'.
--   maxDiff :: [Int] -> Int
--   maxDiff [1,2,3,5,1] => 4
-- - Define 'maxMinDiff' that returns the pair (min_difference,max_difference).


maxDiff :: [Int] -> Int
maxDiff xs = maximum $ map abs $ zipWith (-) xs (tail xs)

-- maxDiff xs = maximum $ map (abs . (fst - snd)) $ zip xs (tail xs)

-- maxDiff xs = maximum . map (abs . uncurry (-)) $ zip xs (tail xs)

maxMinDiff :: [Int] -> (Int, Int)
maxMinDiff xs = (minimum diff, maximum diff)
  where diff = map abs $ zipWith (-) xs (tail xs)

-- 2.2.
-- - Define 'studentsPassed' that takes as input a list [(NameSurname,Score)] and
--   returns the names of all students who scored at least 50% of the maximum
--   score.

studentsPassed :: [(String, Double)] -> [String]
studentsPassed xs = map fst $ filter ((>=m/2) . snd) xs
    where m = maximum $ map snd xs


-- EXERCISE 3 =====================================================

-- 3.1.
-- - Define 'isTitleCased' that checks whether every word in a string is
--   capitalized.
--   isTitleCased :: String -> Bool
--   isTitleCased "University Of Zagreb" => True

isTitleCased :: String -> Bool
isTitleCased = all isUpper . map head . words

-- 3.2.
-- - Define 'sortPairs' that sorts the list of pairs in ascending order with
--   respect to the second element of a pair.

sortPairs :: (Ord b, Ord a) => [(a,b)] -> [(a,b)]
sortPairs = map swap . sort . map swap

-- 3.3.
-- - Define 'filename' that extracts the the name of the file from a file path.
--   filename :: String -> String
--   filename "/etc/init/cron.conf" => "cron.conf"

filename :: String -> String
filename = reverse . takeWhile (/='/') . reverse

-- 3.4.
-- - Define 'maxElemIndices' that returns the indices of the maximum element in a
--   list. Return "empty list" error if the list is empty.
--   maxElemIndices :: Ord a => [a] -> [Int]
--   maxElemIndices [1,3,4,1,3,4] => [2,5]

maxElemIndices :: Ord a => [a] -> [Int]
maxElemIndices [] = error "empty list"
maxElemIndices xs = elemIndices (maximum xs) xs

-- EXERCISE 4 =====================================================

-- 4.1.
-- - Define 'elem' using 'foldr'.

elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr (\y acc -> x==y || acc) False


-- 4.2.
-- - Define 'reverse' using 'foldr'.

reverse :: [a] -> [a]
reverse = foldr (\x acc -> acc ++ [x]) []

-- 4.3.
-- - Using 'foldr' define 'nubRuns' that removes consecutively repeated elements
--   from a list.
--   nubRuns :: Eq a => [a] -> [a]
--   nubRuns "Mississippi" => "Misisipi"

nubRuns :: Eq a => [a] -> [a]
nubRuns = foldr (\x acc -> if acc /= [] && x == head acc then acc else x:acc) []


-- === EXERCISE 5 =================================================================

-- 5.1.
-- - Write 'reverse' using 'foldl'.
--   reverse' :: [a] -> [a]

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []


-- 5.2.
-- - Using 'foldl' define function 'sumEven' from problem 1.1.

sumEven' :: [Integer] -> Integer
sumEven' xs = foldl (\x (a, b) -> if odd b then a + x else x) 0 (zip xs [0..])

-- 5.3.
-- - Using 'foldl' define maxUnzip :: [(Int,Int)] -> (Int,Int)
--   that returns the maximum element at first position in a pair and maximum
--   element at second position in the pair. In other words, the function should
--   be equivalent to:
--     maxUnzip zs = (maximum xs, maximum ys)
--       where (xs,ys) = unzip zs
--   Return "empty list" error if the list is empty.

maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip [] = error "empty list"
maxUnzip (x:xs) = foldl step x xs
  where
    step (x, y) (a, b)
      | x > a && y > b = (x,y)
      | x > a && y < b = (x,b)
      | x < a && y > b = (a,y)
      | otherwise      = (a,b)
