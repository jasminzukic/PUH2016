import           Data.Char
import           Data.List

-- TASK 1 ===========================================

intercalate' :: [a] -> [[a]] -> [a]
intercalate' xs (ys:yss) = intercalate xs yss ys
    where
        intercalate _ _ []         = []
        intercalate _ [] ys        = ys
        intercalate xs (ys:yss) zs = intercalate xs yss (zs ++ xs ++ ys)

-- TASK 2 ===========================================


chunk :: Int -> [a] -> [[a]]
chunk 0 _ = []
chunk _ [] = []
chunk n xs = take n xs:rest
    where
        rest = chunk n (drop n xs)

chunkBy :: [Int] -> [a] -> [[a]]
chunkBy [] _ = []
chunkBy _ [] = []
chunkBy (i:is) xs
    | i == 0    = rest
    | otherwise = take i xs:rest
    where
        rest = chunkBy is (drop i xs)

chunkInto :: Int -> [a] -> [[a]]
chunkInto 0 _  = []
chunkInto _ [] = []
chunkInto n xs = chunkInto' len xs
    where
        len = length xs `div` n :: Int

        chunkInto' :: Int -> [a] -> [[a]]
        chunkInto' len xs
            | length xs < (2*len) = [xs]
            | otherwise           = take len xs:rest
            where
                rest = chunkInto' len (drop len xs)


-- TASK 3 ===================================================

cycleMap :: [a -> b] -> [a] -> [b]
cycleMap [] _ = []
cycleMap fs xs = cycleMap' fs xs 0
    where
        cycleMap' :: [a -> b] -> [a] -> Int -> [b]
        cycleMap' _ [] _ = []
        cycleMap' fs (x:xs) n = (fs!!n) x:cycleMap' fs xs ((n+1) `mod` length fs)

-- TASK 4 ==================================================

reduce :: (a -> b -> a) -> a -> [b] -> a
reduce _ n []     = n
reduce f n (x:xs) = reduce f (f n x) xs

reduce1 :: (a -> a -> a) -> [a] -> a
reduce1 _ [] = error "reduce1 got an empty list"
reduce1 f (x:xs) = reduce f x xs
    where
        reduce _ n []     = n
        reduce f n (x:xs) = reduce f (f n x) xs


scan :: (a -> b -> a) -> a -> [b] -> [a]
scan _ n []     = [n]
scan f n (x:xs) = n:scan f (f n x) xs


rreduce :: (a -> b -> b) -> b -> [a] -> b
rreduce _ n [] = n
rreduce f n xs = rreduce f (f (last xs) n) $ init xs

rreduce1 :: (a -> a -> a) -> [a] -> a
rreduce1 _ [] = error "rreduce1 got an empty list"
rreduce1 f xs = rreduce f (last xs) $ init xs
    where
        rreduce _ n [] = n
        rreduce f n xs = reduce f (f n (last xs)) $ init xs


rscan :: (a -> b -> b) -> b -> [a] -> [b]
rscan _ n [] = [n]
rscan f n xs = rscan f (f (last xs) n) (init xs) ++ [n]


-- TASK 5 ===========================================

type Tolerance = Double

newton :: Tolerance -> Double -> Double
newton t x
    | x < 0     = error "can't get sqrt of negative number"
    | otherwise = newton' t x 100 -- random initial root
    where
        newton' t x y
            | abs (y' - y) < t = y'
            | otherwise        = newton' t x y'
            where y' = (y + x/y)/2


deriv :: (Double -> Double) -> Double -> Double
deriv f x = (f (x+dx) - f x)/dx
    where dx = 0.00001

-- TASK 6 =============================================

type Operators = [(Char, Int -> Int -> Int)]

basic    = [ ('+', (+))
           , ('-', (-)) ] :: Operators

standard = [ ('+', (+))
           , ('-', (-))
           , ('*', (*))
           , ('/', div)
           , ('^', (^)) ] :: Operators

rpnCalc :: String -> Operators -> Int
rpnCalc [] typ = 0
rpnCalc s typ
    | even (length s) && (length s /= 1) = error "Invalid RPN expression"
    | not $ isDigit (s!!2) = calc
                               (s!!2)              -- for normal arangement of arguments (23+5+1-)
                               (digitToInt $ head s)
                               (digitToInt $ s!!1)
                               (drop 3 s)
                               typ
    | otherwise            = calc2
                               (digitToInt $ head 0) -- for inverse arangement of arguments (23+56+-)
                               (s!!3)
                               (digitToInt $ s!!1)
                               (digitToInt $ s!!2)
                               (drop 4 s)
                               typ
    where
        ifNotOp :: Char -> Bool
        ifNotOp f = not (any (\(a, b) -> a == f) typ)

        getOp :: Char-> (Int -> Int -> Int)
        getOp f = snd $ head $ filter (\(a,b) -> a == f) typ

        calc :: Char -> Int -> Int -> String -> Operators -> Int
        calc f x y [] typ
            | ifNotOp f = error $ "Invalid symbol " ++ [f]
            | otherwise = getOp f x y :: Int
        calc f x y xs typ
            | ifNotOp f         = error $ "Invalid symbol " ++ [f]
            | isDigit (xs !! 1) = calc2
                                    (getOp f x y)
                                    (xs!!2)
                                    (digitToInt $ head xs)
                                    (digitToInt $ xs!!1)
                                    (drop 3 xs)
                                    typ
            | otherwise         = calc
                                    (xs!!1)
                                    (getOp f x y)
                                    (digitToInt $ head xs)
                                    (drop 2 xs)
                                    typ
        calc2 :: Int -> Char -> Int -> Int -> String -> Operators -> Int
        calc2 prev f x y xs typ
            | ifNotOp f = error $ "Invalid symbol " ++ [f]
            | otherwise = calc (head xs)
                            prev
                            (getOp f x y)
                            (drop 1 xs)
                            typ
