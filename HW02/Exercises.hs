import Data.Char
import Data.List

-- === EXERCISE 1 ===============================================================

-- Define the following functions using pattern matching.

-- 1.1.
-- - Define 'headHunter xss' that takes the head of the first list element. If 
--   the first element has no head, it takes the head of the second element.
--   If the second element has no head, it takes the head of the third element.
--   If none of this works, the function returns an error.

headHunter :: [[a]] -> a
headHunter ([]:[]:(x:_):_)  = x
headHunter ([]:(x:_):_)     = x
headHunter ((x:_):_)        = x
headHunter _                = error "no heads in first three lists"


-- 1.2.
-- - Define 'firstColumn m' that returns the first column of a matrix.
--   firstColumn [[1,2],[3,4]] => [1,3]
-- - Check what happens if the input is not a valid matrix.

firstColumn :: [[a]] -> [a]
firstColumn m = [ h | (h:_) <- m ]

-- 1.3.
-- - Define 'shoutOutLoud' that repeats three times the initial letter of each
--   word in a string.
--   shoutOutLoud :: String -> String
--   shoutOutLoud "Is anybody here?" => "IIIs aaanybody hhhere?"

shoutOutLoud :: String -> String
shoutOutLoud msg = unwords [ h:h:h:t | (h:t) <- words msg]

-- shoutOutLoud msg = concat [ h:h:h:t ++ " " | (h:t) <- words msg ]


-- === EXERCISE 2 ===============================================================

-- Solve the following exercises using pattern matching and local definitions,
-- wherever appropriate.

-- 2.1.
-- - Define 'pad' that pads the shorter of two the strings with trailing spaces 
--   and returns both strings capitalized.
--   pad :: String -> String -> (String, String)
--   pad "elephant" "cat" => ("Elephant", "Cat     ")

pad :: String -> String -> (String, String)
pad (x:xs) (y:ys) = (toUpper x : adjust len xs, toUpper y : adjust len ys)
    where 
        len         = max (length xs) (length ys)
        adjust n s  = s ++ replicate (n - length s) ' '

-- 2.2.
-- - Define 'quartiles xs' that returns the quartiles (q1,q2,q3) of a given list.
--   The quartiles are elements at the first, second, and third quarter of a list
--   sorted in ascending order. (You can use the built-int 'splitAt' function and
--   the previously defined 'median' function.)
--   quartiles :: [Int] -> (Double,Double,Double)
--   quartiles [3,1,2,4,5,6,8,0,7] => (1.5, 4.0, 6.5)

median :: (Integral a, Fractional b) => [a] -> b
median [] = error "median: Empty List"
median xs 
    | odd l = realToFrac $ ys !! h
    | otherwise = realToFrac (ys !! h + ys !! (h-1)) / 2
    where
        l = length xs
        ys = sort xs
        h = l `div` 2


quartiles :: [Int] -> (Double,Double,Double)
quartiles xs = (firstQ, secondQ, thirdQ)
    where
        list = sort xs
        firstQ = median $ fst $ splitAt (div (length xs) 2) list
        secondQ = median list
        thirdQ = median $ snd $ splitAt ((div (length xs) 2)+1) list




-- === EXERCISE 3 ===============================================================

-- Redo Exercise 2 using 'let' instead of 'where'.

pad' :: String -> String -> (String, String)
pad' (x:xs) (y:ys) = let
    len = max (length xs) (length ys)
    adjust n s  = s ++ replicate (n - length s) ' ' in
        (toUpper x : adjust len xs, toUpper y : adjust len ys)

quartiles' :: [Int] -> (Double,Double,Double)
quartiles' xs = let
    list = sort xs
    firstQ = median $ fst $ splitAt (div (length xs) 2) list
    secondQ = median list
    thirdQ = median $ snd $ splitAt ((div (length xs) 2)+1) list in
        (firstQ, secondQ, thirdQ)


-- === EXERCISE 4 ===============================================================

-- 4.1.
-- - Write a function that takes in a pair (a,b) and a list [c] and returns the
--   following string:
--   "The pair [contains two ones|contains one one|does not contain a single one]
--   and the second element of the list is <x>"

dajlol :: (Num a, Num b, Eq a, Eq b, Show c) => (a,b) -> [c] -> String
dajlol (x,y) [] = 
    "The pair " ++ case (x,y) of
        (1,1) -> "contains two ones"
        (1,_) -> "contains one one"
        (_,1) -> "contains one one"
        (_,_) -> "does not contain a single one"
    ++ " and there are no elements in the list."
dajlol (x,y) (_:[]) = 
    "The pair " ++ case (x,y) of
        (1,1) -> "contains two ones"
        (1,_) -> "contains one one"
        (_,1) -> "contains one one"
        (_,_) -> "does not contain a single one"
    ++ " and there is no second element in the list."
dajlol (x,y) (_:z:_) = 
    "The pair " ++ case (x,y) of
        (1,1) -> "contains two ones"
        (1,_) -> "contains one one"
        (_,1) -> "contains one one"
        (_,_) -> "does not contain a single one"
    ++ " and the second element of the list is " ++ show z