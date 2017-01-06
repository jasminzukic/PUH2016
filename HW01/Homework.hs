import Data.Char (digitToInt)

-- 1st task ===================================================================================

norm (a,b) = sqrt (a^2 + b^2)

normalize (a,b)
    | a == 0 && b == 0  = error "Cannot normalize null vector"
    | otherwise         = (a / norm (a,b), b / norm (a,b))

scalarMult k (a,b) = (k*a,k*b)

dot (a,b) (c,d) = a*c + b*d

cos' (a,b) (c,d)
    | (a == 0 && b == 0) || (c == 0 && d == 0) = error "Null vector given"
    | otherwise                                = dot (a,b) (c,d) / (norm (a,b) * norm (c,d))


-- had to add a range of tolerance because of float imprecision

areParallel (a, b) (c, d) = (cos' (a, b) (c, d) < 1.00000001) && (cos' (a, b) (c, d) > 0.99999999)


-- 2nd task ================================================================================

splitAt' :: Int -> [a] -> ([a],[a])
splitAt' n xs
    | n < 1 || n >= length xs   = error "n is out of range"
    | otherwise                 = (take n xs, drop n xs)

-- 3rd task ================================================================================

-- zipWith works like zip but uses any given function instead of tupling
-- zipWith requires 3 arguments. 1st - function, 2nd - a list, 3rd - a list
-- the function is just a regular * operator
-- first list is just an infinite list of 1 and 2. The order depends on the length of the second array

doubleEverySnd :: [Int] -> [Int]
doubleEverySnd xs 
    | odd $ length xs   = zipWith (*) (cycle [1,2]) xs
    | otherwise         = zipWith (*) (cycle [2,1]) xs

-- Recursively adds the least significant digit and the rest of the number

sumOfDigits :: Int -> Int
sumOfDigits n 
    | n < 10        = n
    | otherwise     = (n `mod` 10) + sumOfDigits (n `div` 10)

-- converts the list using sumOfDigits as a function for each element of the list

convertList :: [Int] -> [Int]
convertList xs = [ sumOfDigits x | x <- doubleEverySnd xs]

-- this luhn algorithm implementation accepts only list like this: "52356145" and ['1', '2', '3']

luhn :: [Char] -> Bool
luhn xs = (sum $ convertList $ map digitToInt xs) `mod` 10 == 0

-- 4th task ================================================================================

factorize :: Int -> [Int]
factorize n = [x | x <- [1..n], n `mod` x == 0]

-- prime number are the only ones that have just 2 elements in it's factor list: [1, thatNumber]

primes :: Int -> [Int]
primes n = take n [x | x <- [1..], (length $ factorize x) == 2]