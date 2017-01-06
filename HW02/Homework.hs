import Data.List
import Data.Map (fromListWith, toList)

-- 1st Task =========================================================

isWellFormed :: [[Int]] -> Bool
isWellFormed [[]] = False
isWellFormed (xs:[]) = True         -- end case of recursion
isWellFormed (xs:xss) = (length xs == length (head xss)) && isWellFormed xss


size :: [[Int]] -> (Int,Int)
size xss
    | not $ isWellFormed xss    = error "Matrix is malformed"
    | otherwise                 = (length xss, length $ head xss)


getElement :: [[Int]] -> Int -> Int -> Int
getElement xss x y
    | not $ isWellFormed xss            = error "Matrix is malformed"
    | x < 0 || x >= (fst $ size xss) || 
      y < 0 || y >= (snd $ size xss)    = error "Index out of bounds"
    | otherwise                         = xss !! x !! y


getRow :: [[Int]] -> Int -> [Int]
getRow xss x
    | not $ isWellFormed xss            = error "Matrix is malformed"
    | x < 0 || x >= (fst $ size xss)    = error "Index out of bounds"
    | otherwise                         = xss !! x


getCol :: [[Int]] -> Int -> [Int]
getCol xss y
    | not $ isWellFormed xss            = error "Matrix is malformed"
    | y < 0 || y >= (snd $ size xss)    = error "Index out of bounds"
    | otherwise                         = [ xs !! y | xs <- xss ]


addMatrices :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices xss yss
    | size xss /= size yss      = error "Matrices are not of equal size"
    | otherwise                 = 
        [[getElement xss x y + getElement yss x y  -- automatically checks if it's malformed
            | y <- [0..((snd $ size xss)-1)]]    -- double list comprehensions
                | x <- [0..((fst $ size xss)-1)]] 


transpose' :: [[Int]] -> [[Int]]
transpose' xss = [getCol xss y | y <- [0..((snd $ size xss)-1)]]


multMatrices :: [[Int]] -> [[Int]] -> [[Int]]
multMatrices xss yss
    | (snd $ size xss) /= (fst $ size yss)  = error "Incompatible matrix dimensions"
    | otherwise                             = 
        [[sum (zipWith (*) (getRow xss x) (getCol yss y)) 
            | y <- [0..((snd $ size xss)-1)]]
                | x <- [0..((fst $ size xss)-1)]]

-- [1 2] *  [7  8] = [1*7+2*9  1*8+2*10]
-- [3 4]    [9 10]   [3*7+4*9  3*8+4*10]
-- [5 6]             [5*7+6*9  5*8+6*10]



-- 2nd Task ==================================================

type Key = Int
type Value = String
type Entry = (Key, Value)
type Dictionary = [Entry]
type Frequency = [(Value, Int)]

exists :: Key -> Dictionary -> Bool
exists _ []         = False
exists n ((k,v):xs) = n == k || exists n xs


get :: Dictionary -> Key -> Value
get xs@((k,v):ys) n
    | not $ exists n xs = error ("key " ++ show n ++ " not found")
    | otherwise         = if n == k then v else get ys n


insert' :: Entry -> Dictionary -> Dictionary
insert' ent [] = ent:[]
insert' (k1,v1) (ws:(k,v):ys)
    | k1 == k   = ws:(k1,v1):ys
    | otherwise = ws:(k,v):(insert' (k1,v1) ys)

delete' :: Key -> Dictionary -> Dictionary
delete' _ [] = []
delete' n (ws:(k,v):ys)
    | n == k    = ws:ys
    | otherwise = ws:(k,v):(delete' n ys)


freq :: Dictionary -> Frequency
freq [] = error "Dictionary is empty"
freq xss = toList (fromListWith (+) [(v, 1) | (k,v) <- xss])

-- don't ask. It works. Move along :D

-- 3rd Task ========================================================

 
largestMultiple :: String -> Int
largestMultiple s
    | not (hasZero s && isDivBy3 s) = error "No such number"
    | otherwise = maximum [ read x :: Int | x <- permutations s]
    where   
        hasZero s = '0' `elem` s
        isDivBy3 s = (mod (sum $ convert s) 3) == 0
        convert s = map (read . (:"")) s :: [Int]

-- TASK 4 =================================================

undefined' = error "Prelude.undefined"

undefined'' = undefined''

only :: a -> b -> a
only a b = a