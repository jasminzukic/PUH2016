import Data.Char
import Data.List

-- === EXERCISE 0 ===============================================================

-- 1.1. 
-- - Define 'concat3' that concatenates three strings, but drops the middle one
--   if it's shorter than 2 characters (use 'length' function).

concat3 s1 s2 s3
    | length s2 < 2   = s1 ++ s3
    | otherwise      = s1 ++ s2 ++ s3


-- 1.2.
-- - Give a simpler definition of 'showSalary', using only one if-then-else
--   construct.
-- - Additionally check that salary is non-negative. If it's negative, return an
--   adequate message.

showSalary' amount bonus = 
    if amount < 0 
        then "The amount is negative!"
        else "Salary is " ++ show amount 
                          ++ if bonus /= 0 then ", and the bonus is " ++ show bonus else ""


-- === EXERCISE 1 ===============================================================

-- 1.1.
-- - Define a function that returns a list without the first three elements and 
--   last three elements.

withoutThreeElements xs = tail $ tail $ tail $ init $ init $ init xs

-- 1.2.
-- - Define a function 'initals s1 s2' that takes a person's name and a surname 
--   as input and returns a string consisting of person's initials.
--   initials "James" "Bond" => "J. B."

initials s1 s2 = [head s1] ++ ". " ++ [head s2] ++ "."

-- 1.3.
-- - Define a function that concatenates two strings, so that the longest string
--   always comes first.

concatLongestFirst s1 s2
    | length s1 > length s2	= s1 ++ s2
    | otherwise             = s2 ++ s1

-- 1.4.
-- - Define a function 'safeHead' that returns an empty list if 'l' is an empty
--   list, otherwise it returns its first element wrapped inside a singleton list.

-- I made it to work with lists of lists (like ["abcd"])
-- the type is defined as written below

safeHead :: [a] -> [a]
safeHead l
    | null l    = []
    | otherwise = [head l]

-- 1.5.
-- - Define a function 'hasDuplicates' that checks whether a list contains
--   duplicate elements (use 'nub').

-- hasDuplicates xs = (length $ nub xs) < (length xs)

hasDuplicates xs = xs /= nub xs

-- === EXERCISE 2 ===============================================================

-- 2.1
-- - Redefine 'doublesFromTo' so that it also works when b<a.

doublesFromTo a b
    | b < a     = doublesFromTo b a
    | otherwise = [x*2 | x <- [a..b]]

-- 2.2.
-- - Redefine 'ceasarCode n xs' so that it shifts all letters a specified number 
--   of positions 'n', converts all input to lowercase, and ensures that letters 
-- 	  remain within the ['a'..'z'] interval.

-- 'a' = 97, 'z' = 122

caesarCode n xs = [chr $ (((ord $ toLower c) + n) `mod` (ord 'z' + 1) 
                            + (if ((ord $ toLower c) + n) > ord 'z' then ord 'a' else 0)) 
                            | c <- xs, c /= ' ']


-- === EXERCISE 3 ===============================================================

-- 3.1.
-- - Define 'letterCount' that computes the total number of letters in a string,
--   thereby ignoring the whitespaces and all words shorter than three letters.
--   You can use 'totalLength'.

letterCount :: String -> Int
letterCount s = length $ concat [w | w <- words s, length w > 3]

-- 3.2
-- - Redefine 'isPalindrome' so that it's case insensitive and works correctly 
--   for strings that contain whitespaces.

lowerCase xs = [toLower c | c <- xs]
isPalindrome s = (reverse $ concat [lowerCase w | w <- words s]) == (concat [lowerCase w | w <- words s])

-- 3.3.
-- - Define 'flipp xss' that takes a list of lists, reverts each individual list,
--   and concatenates all of them, but in the reverse order.
--   flipp ["water","is","warm"] -> "retawsimraw"

flipp xss = concat [reverse xs | xs <- xss]

-- === EXERCISE 4 ===============================================================

-- 4.1.
-- - Define 'inCircle r x y' that returns the coordinates of all points within
--   the ([-10..10],[-10..10]) interval that fall inside a circle of radius
--   'r' with center '(x,y)'.
-- - Redefine the function so that it takes the resolution of the grid as an 
--   additional argument.

inCircle r x y res = [(a, b) | a <- [(-10), ((-10) + res)..10], b <- [(-10),((-10) + res)..10], (a-x)^2 + (b-y)^2 <= r^2]

-- 4.2.
-- - Define 'steps xs' that, given a list xs=[x1,x2,..], generates the pairs
--   [(x1,x2),(x2,x3),...]. Hint: have a look at 'pairs5'.

steps xs = zip xs $ tail xs

-- === EXERCISE 5 ===============================================================

-- 5.1.
-- - Define 'indices x xs' that returns the indices of element 'x' in list 'xs'
--   (if 'x' appears multiple times, there will be a number of such indices).
--   indices 'a' "alphabet" => [1,5]

indices x xs = [ fst ix | ix <- zip [1..] xs, snd ix == x]

-- 5.2.
-- - Define 'showLineNumbers s' that prefixes all lines from string 's' with a
--   line number.
--   showLineNumbers "first line\nsecond line" => "1 first line\n2 second line\n"

showLineNumbers s = [show (fst x) ++ " " ++ snd x| x <- zip [1..] $ lines s]

-- 5.3.
-- - Define 'haveAlignment xs ys' that returns 'True' if 'xs' and 'ys' have
--   any identical elements that are aligned (appear at the same position in
--   both lists).
-- - Define 'common xs ys' that returns the aligned subsequences.
--   haveAlignment "water" "fire" => True
--   common "witer" "fire" => "ie"

haveAlignment xs ys = not $ null $ common xs ys

common xs ys = [ fst x | x <- zip xs ys, fst x == snd x]


-- === EXERCISE 6 ===============================================================

-- Without using the ':t' command, determine the types of the following 
-- functions:

foo10 :: String -> [[Char]]
foo10 w = [x ++ y | x <- lines w, y <- lines w]

foo11 :: String -> [(String, String)]
foo11 w = [(x,y) | x <- lines w, y <- lines w]

foo12 :: String -> [[Char]]
foo12 w = [y : x | x <- lines w, y <- w]

foo13 :: String -> [([Char], String)]
foo13 w = [(y:x, w) | x <- lines w, y <- w]

foo14 :: [Char] -> [(Char, Bool)]
foo14 w = [(x, x=='a') | x <- w ]

foo15 :: [Char] -> [Char]
foo15 s = tail [ c | c <- s, isLower c ]

foo16 :: [Char] -> [(Char, Char)]
foo16 s = zip [ c | c <- s, isLower c ] "Haskell"

foo17 :: Int -> Char -> [Char]
foo17 n c = reverse $ drop n $ c : "Haskell" 

foo18 :: String -> String
foo18 xs = last $ words xs

foo19 :: Char -> [Char] -> [Char]
foo19 x z = x : 'y' : z


-- === EXERCISE 7 ===============================================================

-- Without using the ':t' command, determine the types of the following 
-- functions:

foo20 :: [a] -> [a]
foo20 xs = tail xs ++ [head xs]

foo21 :: [a] -> (a, [a])
foo21 xs = (head xs, tail xs)

foo22 :: a -> [a] -> [a]
foo22 x xs = x:xs

foo23 :: [a] -> [a]
foo23 l = init $ tail l

foo24 :: [[a]] -> [a] -> [a]
foo24 xss ys = concat xss ++ ys

foo25 :: [[a]] -> [b] -> (a, b)
foo25 xss ys = (head $ concat xss, head ys)

foo26 :: [[[a]]] -> a
foo26 xs = head $ concat $ concat xs

foo27 :: [a] -> [[a]]
foo27 cs = [[c1,c2] | c1 <- cs, c2 <- cs]

foo28 :: [[a]] -> [[a]]
foo28 cs = [concat [c1,c2] | c1 <- cs, c2 <- cs]

foo29 :: [a] -> [a]
foo29 cs = concat [[c1,c2] | c1 <- cs, c2 <- cs]


-- === EXERCISE 8 ===============================================================

-- Without using the ':t' command, determine the types of the following 
-- functions:

foo30 :: Eq a => a -> [a] -> a
foo30 x ys = if x==head ys then x else last ys

foo31 :: Ord a => a -> [a] -> a
foo31 x ys = if x < head ys then x else last ys

foo32 :: Eq a => [a] -> [[a]] -> a
foo32 xs yss = if xs==head yss then head xs else last xs

foo33 :: (Enum a, Num a) => Bool -> [b] -> [(a,b)]
foo33 x ys = if x then zip [1..9] ys else []

foo34 :: (Enum a, Num a) => String -> [(a, String)]
foo34 w = zip [0..] (lines w)

foo35 :: (Fractional a, Integral a) => a -> a -> a
foo35 x y = if odd x then y else x / 10

foo36 :: Ord a => [a] -> Bool
foo36 xs = sort xs == xs

foo37 :: (Show a, Show b) => a -> [[b]] -> [Char]
foo37 x xs = show x ++ (show $ concat xs)

foo38 :: Num a => [[a]] -> a
foo38 xs = sum $ concat xs

foo39 :: (Num a, Ord a) => [a] -> [[a]] -> a
foo39 xs yss = sum $ [min x y | x <- xs, ys <- yss, y <- ys]
