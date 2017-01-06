import Data.List
import Data.Ord
import Control.Monad


data Point = Point Double Double
  deriving Show
data Shape2 = Circle2 Point Double | Rectangle2 Point Point
  deriving Show

myPoint = Point 3 4
myCircle2 = Circle2 (Point 5 5) 10
myRectangle2 = Rectangle2 (Point 5 5) (Point 10 10)


-- EXERCISE 1 ========================

-- 1.1.
-- - Define a 'Date' structure with the appropriate fields.
-- - Define a function that shows a date in the DD.MM.YYYY format (without
--   leading zeroes).
--   showDate :: Date -> String

data Date = Date Int Int Int

showDate :: Date -> String
showDate (Date d m y) = show d ++ "." ++ show m ++ "." ++ show y


--  | d < 10 && m < 10 = "0" ++ show d ++ ":0" ++ show m ++ ":" ++ show y
--  | m < 10           = show d ++ ":0" ++ show m ++ ":" ++ show y
--  | d < 10           = "0" ++ show d ++ ":" ++ show m ++ ":" ++ show y
--  | otherwise        = show d ++ ":" ++ show m ++ ":" ++ show y


-- 1.2.
-- - Define a function
--   translate :: Point -> Shape2 -> Shape2
--   that translates a shape into the direction of vector (x,y).

translate :: Point -> Shape2 -> Shape2
translate (Point x y) (Circle2 (Point a b) r) = Circle2 (Point (a+x) (b+y)) r
translate (Point x y) (Rectangle2 (Point a b) (Point c d)) = Rectangle2 (Point (a+x) (b+y)) (Point (c+x) (d+y))


-- 1.3.
-- - Write a function 'inShape' that tests whether a point is contained within a
--   given shape (or is on its border).
--   inShape :: Shape2 -> Point -> Bool
-- - Write a function 'inShapes' that tests if the point is within any shape from
--   the list of shapes.
--   inShapes :: [Shape2] -> Point -> Bool

inShape :: Shape2 -> Point -> Bool
inShape (Circle2 (Point a b) r) (Point x y) = abs (a - x) <= r && abs (b-y) <= r
inShape (Rectangle2 (Point a b) (Point c d)) (Point x y)
  | (x > a && x < c || x < a && x > c) ||
    (y > b && y < d || y < b && y > d) = True
  | otherwise                          = False

inShapes :: [Shape2] -> Point -> Bool
inShapes [] _ = False
inShapes (x:xs) p = inShape x p || inShapes xs p


-- 1.4.
-- - Define your type 'Vehicle' that can be a 'Car', 'Truck',
--   'Motorcycle', or 'Bicycle'. The first three store a name of the manufacturer
--   (String) and horsepower (Double).
-- - Write a function 'totalHorsepower' that adds up the horsepower of the
--   vehicles, assuming that bicycle's horsepower is 0.2.

data Vehicle = Car String Double | Truck String Double | Motorcycle String Double | Bicycle
  deriving Show

totalHorsepower :: [Vehicle] -> Double
totalHorsepower xs = totalHorsepower' xs 0
  where
    totalHorsepower' :: [Vehicle] -> Double -> Double
    totalHorsepower' [] d = d
    totalHorsepower' (Car _ h:xs) d        = totalHorsepower' xs (d+h)
    totalHorsepower' (Truck _ h:xs) d      = totalHorsepower' xs (d+h)
    totalHorsepower' (Motorcycle _ h:xs) d = totalHorsepower' xs (d+h)
    totalHorsepower' (Bicycle:xs) d        = totalHorsepower' xs (d+0.2)

-- ======================================

data Level = Bachelor | Master | PhD deriving (Show,Eq)

data Student = Student
  { firstName  :: String
  , lastName   :: String
  , studentId  :: String
  , level      :: Level
  , avgGrade   :: Double } deriving Show

nekiStudent = Student "Ivan" "Bartolec" "0036472358" Master 2.1
najStudent = Student "Mijo" "Mijic" "1414" Master 4.9
sveStudenti = [nekiStudent, najStudent]

-- EXERCISE 2 ===================

-- 2.1.
-- - Define a function that increases the average grade of the student by 1.0,
--   but not above 5.0.
--   improveStudent :: Student -> Student

improveStudent :: Student -> Student
improveStudent (Student fn ln i l g) = Student fn ln i l (min 5 (g + 1))


-- 2.2.
-- - Write a function to compute the average grade of students for the different
--   study levels.
--   avgGradePerLevels :: [Student] -> (Double, Double, Double)

avgGradePerLevels :: [Student] -> (Double, Double, Double)
avgGradePerLevels ss =
  ( avg $ map avgGrade $ filter ((==Bachelor) . level) ss
  , avg $ map avgGrade $ filter ((==Master) . level) ss
  , avg $ map avgGrade $ filter ((==PhD) . level) ss
  )
    where
      avg :: [Double] -> Double
      avg xs = sum xs / genericLength xs

-- 2.3.
-- - Write a function that returns a list of matriculation numbers for a given
--   study level, sorted by average grade in descending order.
--   rankedStudents :: Level -> [Students] -> [String]

rankedStudents :: Level -> [Student] -> [String]
rankedStudents l xs = map studentId $ sortBy (comparing avgGrade) $ filter ((==l) . level) xs

-- 2.4.
-- - Write a function
--   addStudent :: Student -> [Student] -> [Student]
--   that adds a student to a list of students. If a student with an identical
--   matriculation number already exists in the list, the function should return an
--   error.

checkStudent :: Student -> [Student] -> Bool
checkStudent s [] = False
checkStudent s@(Student _ _ i _ _) (Student _ _ i2 _ _:xs)
  | i == i2   = True
  | otherwise = checkStudent s xs

addStudent :: Student -> [Student] -> [Student]
addStudent s xs
  | checkStudent s xs = error "student already exists!"
  | otherwise         = s:xs

-- EXERCISE 3 ====================

-- 3.1.
-- - Define your own parametrized type 'MyTriplet' that contains the values of
--   three different types. Do this using a record.
-- - Define a function
--   toTriplet :: MyTriplet a b c -> (a, b, c)
--   that converts a 'MyTriplet' value into an ordinary triplet.

data MyTriplet a b c = MyTriplet
  { frst :: a
  , scnd :: b
  , thrd :: c } deriving Show

toTriplet :: MyTriplet a b c -> (a, b, c)
toTriplet (MyTriplet frst scnd thrd) = (frst, scnd, thrd)


-- 3.2.
-- - Define a function (Employee - salary :: Maybe Double, name :: String) deriving Show
--   totalSalaries :: [Employee] -> Double
--   that sums the known salaries of employees (salaries that are not 'Nothing').

data Employee = Employee
  { salary :: Maybe Double
  , name :: String } deriving Show

totalSalaries :: [Employee] -> Double
totalSalaries xs = totalSalaries' xs 0
  where
    totalSalaries' :: [Employee] -> Double -> Double
    totalSalaries' [] total = total
    totalSalaries' (Employee Nothing n:xs) total = totalSalaries' xs total
    totalSalaries' (Employee (Just d) n:xs) total = totalSalaries' xs (total + d)

-- 3.3.
-- - Write a function 'addStudent2' that works like 'addStudent' from problem 2.4
--   but returns a 'Maybe' type instead of an error.
--   addStudent2 :: Student -> [Student] -> Maybe [Student]
-- - Write 'addStudent3' that returns an 'Either'.

addStudent2 :: Student -> [Student] -> Maybe [Student]
addStudent2 s xs
  | checkStudent s xs = Nothing
  | otherwise         = Just (s:xs)

-- ================================

data Sex = Male | Female deriving (Show,Read,Ord,Eq)

data Person = Person {
  idNumber :: String,
  forename :: String,
  surname  :: String,
  sex      :: Sex,
  age      :: Int,
  partner  :: Maybe Person,
  children :: [Person] }

pero  = Person "2323" "Pero"  "Perić" Male   45 (Just ana)   [marko]
ana   = Person "3244" "Ana"   "Anić"  Female 43 (Just pero)  [marko,iva]
marko = Person "4341" "Marko" "Perić" Male   22 (Just maja)  []
maja  = Person "7420" "Maja"  "Majić" Female 20 (Just marko) []
iva   = Person "4642" "Iva"   "Ivić"  Female 16 Nothing      []


data Person2 = Person2 {
  personId2 :: String,
  forename2 :: String,
  surname2  :: String,
  sex2      :: Sex,   --- data Sex = Male | Female deriving (Show,Read,Eq,Ord)
  mother2   :: Maybe Person2,
  father2   :: Maybe Person2,
  partner2  :: Maybe Person2,
  children2 :: [Person2] } deriving (Show,Read,Eq,Ord)

john = Person2 "123" "John" "Doe" Male Nothing Nothing (Just jane) []
jane = Person2 "623" "Jane" "Fox" Female (Just ann) Nothing (Just john) [lucy]
ann  = Person2 "343" "Ann"  "Doe" Female Nothing Nothing Nothing [jane]
lucy = Person2 "234" "Lucy" "Doe" Female (Just jane) Nothing Nothing []


-- EXERCISE 4 ====================

-- 1.1.
-- - Define a function

foo = foo

-- 1.2.
-- - Define a function
--   parentCheck :: Person2 -> Bool
--   that checks whether the given person is one of the children of its parents.

fathersChildren :: Person2 -> [Person2]
fathersChildren p = case father2 p of
  Just f -> children2 f
  Nothing -> []

mothersChildren :: Person2 -> [Person2]
mothersChildren p = case mother2 p of
  Just f -> children2 f
  Nothing -> []

-- mothersChildren p = maybe [] children2 . mother2

parentCheck :: Person2 -> Bool
parentCheck p = p `elem` fathersChildren p ++ mothersChildren p



-- 1.3.
-- - Define a function
--   sister :: Person2 -> Maybe Person2
--   that returns the sister of a person, if such exists.

sister :: Person2 -> Maybe Person2
sister p = case filter ((Female ==) . sex2) $ mothersChildren p of
  [] -> Nothing
  xs -> Just $ head xs


-- 1.4.
-- - Define a function that returns all descendants of a person.
--   descendant :: Person2 -> [Person2]

descendant :: Person2 -> [Person2]
descendant p = case children2 p of
  [] -> []
  (p@(Person2 _ _ _ _ _ _ _ []):xs) -> p:xs
  (p@(Person2 _ _ _ _ _ _ _ c):xs) -> p:listDescendant c ++ xs

listDescendant :: [Person2] -> [Person2]
listDescendant [] = []
listDescendant (c:cs) = descendant c ++ listDescendant cs

-- ===============================

data MyList a = Empty | Cons a (MyList a) deriving (Show,Read,Ord)

-- Now we can define some lists:

l1 = 1 `Cons` Empty
l2 = 1 `Cons` (2 `Cons` (3 `Cons` Empty))

-- To improve readability, we can define our own infix operator:

infixr 5 -+-
(-+-) = Cons

-- Operator priority (set to 5 in the above example) range from 0 (lowest
-- priority) to 9 (highest priority). E.g., ($) has a priority 0, while (.) has
-- priority 9. You can find out more here:
-- http://www.haskell.org/onlinereport/decls.html#fixity
--
-- We can now write:

l3 = 1 -+- 2 -+- 3 -+- Empty

-- EXERCISE 5 ====================

-- 2.1.
-- - Define
--   listHead :: MyList a -> Maybe a

listHead :: MyList a -> Maybe a
listHead Empty      = Nothing
listHead (Cons x _) = Just x

-- 2.2.
-- - Define a function that works like 'map' but works on a 'MyList' type:
--   listMap :: (a -> b) -> MyList a -> MyList b

listMap :: (a -> b) -> MyList a -> MyList b
listMap f Empty = Empty
listMap f (Cons x xs) = Cons (f x) (listMap f xs)

-- ====================================

data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show)

intTree :: Tree Int
intTree = Node 1 (Node 2 Null Null) (Node 3 Null (Node 4 Null Null))


-- EXERCISE 6 ====================

-- 3.1.
-- - Define a function
--   treeMax :: Ord a => Tree a -> a
--   that finds the maximum element in a tree. Return an error if the tree is
--   empty.

treeMax :: Ord a => Tree a -> a
treeMax Null = error "Empty tree"
treeMax (Node x Null Null) = x
treeMax (Node x Null rb)   = x `max` treeMax rb
treeMax (Node x lb Null)   = x `max` treeMax lb
treeMax (Node x lb rb)     = x `max` treeMax lb `max` treeMax rb

-- 3.2.
-- - Define a function
--   treeToList :: Ord a => Tree a -> [a]
--   that will collect in a list all elements from inner nodes of a tree by doing
--   an in-order (left-root-right) traversal.

treeToList :: Ord a => Tree a -> [a]
treeToList Null               = []
treeToList (Node x lb rb)     = treeToList lb ++ x:treeToList rb

-- 3.3.
-- - Define a function to prune the tree at a given level (root has level 0).
--   levelCut :: Int -> Tree a -> Tree a

levelCut :: Int -> Tree a -> Tree a
levelCut n t = levelCut' n t 0
  where
    levelCut' :: Int -> Tree a -> Int -> Tree a
    levelCut' _ Null _ = Null
    levelCut' n (Node x lb rb) l
     | l == n-1  = Node x Null Null
     | otherwise = Node x (levelCut' n lb (l+1)) (levelCut' n rb (l+1))


-- ============================

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Null = Node x Null Null
treeInsert x t@(Node y l r)
  | x < y     = Node y (treeInsert x l) r
  | x > y     = Node y l (treeInsert x r)
  | otherwise = t


-- EXERCISE 7 ==================

-- 4.1.
-- - Define a function that converts a list into a sorted binary tree.
--   listToTree :: Ord a => [a] -> Tree a

listToTree :: Ord a => [a] -> Tree a
listToTree xs = listToTree' xs Null
  where
    listToTree' xs t = foldl (flip treeInsert) t xs


-- 4.2.
-- - Using 'listToTree' and 'treeToList' defined previously, define these two
--   functions, define:
--   sortAndNub :: Ord a => [a] -> [a]

sortAndNub :: Ord a => [a] -> [a]
sortAndNub = treeToList . listToTree

-- =============================

data Weekday =
  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show,Enum)


-- EXERCISE 8 ==================

-- 5.1.
-- - Define an 'Eq' instance for the 'Weekday' type that works like (==), except
--   that two Fridays are never identical.

instance Eq Weekday where
  Monday    == Monday    = True
  Tuesday   == Tuesday   = True
  Wednesday == Wednesday = True
  Thursday  == Thursday  = True
  Friday    == Friday    = False
  Saturday  == Saturday  = True
  Sunday    == Sunday    = True
  _         == _         = False

-- 5.2.
-- - Define 'Person' as an instance of 'Show' type class so that instead of the
--   values of partners and children only the respective person names are shown,
--   which will enable the print out of an infinite structure of this type.

instance Show Person where
  show (Person _ f s _ _ _ _) = f ++ " " ++ s

-- EXERCISE 9 ===================

-- 6.1.
-- - Define an instance of 'Eq' for 'MyList a' so that two lists are considered
--   equal if they have the same first element.

instance Eq a => Eq (MyList a) where
  Empty == Empty = True
  _     == Empty = False
  Empty == _     = False
  (Cons x _) == (Cons y _) = x == y

-- 6.2.
-- - Define an instance of 'Eq' for 'Tree a' so that two trees are considered
--   equal if they store the same values, regardless of the position of these
--   values in the trees, and regardless of duplicates.

instance Eq a => Eq (Tree a) where
  Null == Null = True
  _    == Null = False
  Null == _    = False
  (Node x _ _) == (Node y lb rb) = x == y
