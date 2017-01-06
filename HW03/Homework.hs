import Data.Char

-- TASK 1 ===========================================================

type RomanNumeral = String
type RomanDigitPair = [(RomanNumeral, Int, Int)]

romanDigitTable :: RomanDigitPair
romanDigitTable =
   [("M" ,  1000, 3),
    ("CM",  900 , 1),
    ("D" ,  500 , 1),
    ("CD",  400 , 1),
    ("C" ,  100 , 3),
    ("XC",  90  , 1),
    ("L" ,  50  , 1),
    ("XL",  40  , 1),
    ("X" ,  10  , 3),
    ("IX",  9   , 1),
    ("V" ,  5   , 1),
    ("IV",  4   , 1),
    ("I" ,  1   , 3)]

isInTable :: String -> Bool
isInTable xs = not $ null [ x | (x,n,c) <- romanDigitTable, x == xs]

getRoman :: Int -> String
getRoman num = head [ x | (x,n,c) <- romanDigitTable, n == num ]

getNumber :: String -> Int
getNumber xs = head [ n | (x,n,c) <- romanDigitTable, x == xs]

getMaxReps :: String -> Int
getMaxReps xs = head [ c | (x,n,c) <- romanDigitTable, x == xs]

isValidRoman :: RomanNumeral -> Bool
isValidRoman [] = False
isValidRoman [a] = isInTable [a]
isValidRoman (a:b:xs)
    | isInTable (a:[b]) = isValidRoman' (a:[b]) xs 1
    | isInTable [a]     = isValidRoman' [a] (b:xs) 1
    | otherwise         = False
    where
        isValidRoman' _ [] _ = True -- first roman has to be True to get to here
        isValidRoman' prev [a] n =
            isInTable [a]
                && ((getNumber [a] < getNumber prev)    -- checks against "CDM", "XCD", "IVC" etc.
                    || (getNumber [a] == getNumber prev)
                        && (getMaxReps [a] > n))        -- checks against "DD", "IIII", "XCXC"
                && not (length prev==2 && a>=head prev)   -- checks against "CDC", "IXI",
        isValidRoman' prev (a:b:xs) n
            |  isInTable (a:[b]) -- is double digit (CM, XL...)
            && getNumber (a:[b]) < getNumber prev -- is double digit smaller than the previous
            && not (length prev==2 && a>=head prev) = isValidRoman' (a:[b]) xs 1
                                 -- checks against "CDCM" "IXXC"...
            |  isInTable (a:[b]) -- is double digit
            && getNumber (a:[b]) == getNumber prev -- is the same as previous
            && getMaxReps (a:[b]) > n -- did we repeat the digit too many times
            && not (length prev==2 && a>=head prev) = isValidRoman' (a:[b]) xs (n+1)

            |  isInTable [a] -- is single digit
            && getNumber [a] < getNumber prev  -- is smaller than the previous
            && not (length prev==2 && a>=head prev) = isValidRoman' [a] (b:xs) 1
                             -- is
            | isInTable [a]
            && getNumber [a] == getNumber prev
            && getMaxReps [a] > n
            && not (length prev==2 && a>=head prev) = isValidRoman' [a] (b:xs) (n+1)

            | otherwise         = False



toRoman :: Int -> RomanNumeral
toRoman 0 = []
toRoman num
    | num >= 4000 || num <= 0   = error "Number cannot be represented"
    | otherwise                 = s ++ toRoman (num - getNumber s)
        where s = head [ x | (x,n,c) <- romanDigitTable, num >= n]


fromRoman :: RomanNumeral -> Int
fromRoman xs
    | not $ isValidRoman xs = error "Not a valid Roman numeral"
    | otherwise             = fromRoman' xs
    where
        fromRoman' [] = 0
        fromRoman' [a]
            | isInTable [a]     = getNumber [a]
        fromRoman' xs@(a:b:bs)
            | isInTable (a:[b]) = getNumber (a:[b]) + fromRoman' bs
            | isInTable [a]     = getNumber [a]   + fromRoman' (b:bs)

-- TASK 2 ===========================================================

prvi :: [(Int,Int)]
drugi :: [(Int,Int)]
prvi = [(2,5),(10,12),(15,17),(8,13)]
drugi = [(8,9),(6,7),(8,12),(4,9)]

zipOne :: [(Int,Int)] -> [(Int,Int,Int,Int,Int)]
zipOne [(_,_)] = []
zipOne ((a,b):(x,y):ys) = (a,b,x,y,distance) : zipOne ((a,b):ys)
    where distance = abs (x-a) + abs (y-b)

zipAll :: [(Int,Int)] -> [(Int,Int,Int,Int,Int)]
zipAll [(_,_)] = []
zipAll ys = zipOne ys ++ zipAll (tail ys)

findSndPoint :: [(Int,Int,Int,Int,Int)] -> (Int,Int) -> Int -> (Int,Int)
findSndPoint combinations (a,b) dis
    | firstTry /= []    = head firstTry
    | secondTry /= []   = head secondTry
    | otherwise         = error "There is no point on that distance"
    where
        firstTry = [ (x,y) | (v,w,x,y,d) <- combinations, v==a, w==b, d==dis]
        secondTry = [ (v,w) | (v,w,x,y,d) <- combinations, x==a, y==b, d==dis]

shortestDistance :: (Int,Int) -> [(Int,Int)] -> Int
shortestDistance _ [] = 0
shortestDistance from@(a,b) xs@((x,y):ys) = smallest
    + shortestDistance (findSndPoint combinations (a,b) smallest) ys
    where
        combinations = zipAll ((a,b):xs)
        smallest = minimum [ d | (i,j,k,l,d) <- combinations, i==a, j==b]



-- TASK 3 ===========================================================

type Probability = Double
type DiscreteRandVar = [(Int,Probability)]

x :: DiscreteRandVar
x = [(1, 0.2), (2, 0.4), (3, 0.1), (4, 0.2), (5, 0.05), (6, 0.05)]

mean :: DiscreteRandVar -> Double
mean [] = 0
mean ((x,p):xs) = fromIntegral x * p + mean xs

mean' :: DiscreteRandVar -> Double
mean' xs = mean'' xs 0
    where
        mean'' [] n = n
        mean'' ((x,p):xs) n = mean'' xs $ fromIntegral x * p + n


variance :: DiscreteRandVar -> Double
variance xs = variance' xs (mean xs)
    where
        variance' [] _ = 0
        variance' ((x,p):xs) m = p*((fromIntegral x - m)^2)
                                + variance' xs m

variance' :: DiscreteRandVar -> Double
variance' xs = variance'' xs (mean xs)
    where
        variance'' xs m = variance xs m 0
            where
                variance [] _ n = n
                variance ((x,p):xs) m n = variance xs m ((p*(fromIntegral x - m)^2)+n)


probabilityFilter :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter _ [] = []
probabilityFilter n ((x,p):xs)
    | p >= n    = x : probabilityFilter n xs
    | otherwise = probabilityFilter n xs

probabilityFilter' :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter' n xs = probabilityFilter'' n (reverse xs) []
    where
        probabilityFilter'' _ [] ys = ys
        probabilityFilter'' n ((x,p):xs) ys
            | p >= n    = probabilityFilter'' n xs (x:ys)
            | otherwise = probabilityFilter'' n xs ys

-- TASK 4 ==========================================================

-- smaller frog has to move 3 times more than the first bigger frog;
-- (2 steps to the third lilypad, 2 steps back, and 2 steps again)
-- the biggest frog has to move only one way so the initial number is 2.
-- 2,6,18,54,162

frogJumps :: Int -> Integer
frogJumps n = frogJumps' n 2 2
    where
        frogJumps' 1 prev s = s
        frogJumps' n prev s = frogJumps' (n-1) (3*prev) (s+(3*prev))
