import Data.Char
import Data.List

-- x and y coordinates
type Position = (Integer,Integer)
data Orientation = West | East | North | South deriving (Eq, Show)
-- Clockwise and Counterclockwise
data TurnDir = CW | CCW deriving (Eq, Show)

data Turtle = Turtle Position Orientation deriving Show

position :: Turtle -> Position
position (Turtle p _) = p

orientation :: Turtle -> Orientation
orientation (Turtle _ o) = o

newTurtle :: Turtle
newTurtle = Turtle (0,0) North

move :: Integer -> Turtle -> Turtle
move n (Turtle (x,y) o)
  | n < 0      = error "Turtles cannot move backwards"
  | o == North = Turtle (x, y+n) o
  | o == East  = Turtle (x+n, y) o
  | o == South = Turtle (x, y-n) o
  | otherwise  = Turtle (x-n, y) o

turn :: TurnDir -> Turtle -> Turtle
turn d (Turtle x o)
  | o == North = if d == CW then Turtle x East else Turtle x West
  | o == East  = if d == CW then Turtle x South else Turtle x North
  | o == South = if d == CW then Turtle x West else Turtle x East
  | otherwise  = if d == CW then Turtle x North else Turtle x South

runTurtle :: [Turtle -> Turtle] -> Turtle -> Turtle
runTurtle moves t = foldl (\t move -> move t) t moves

-- runTurtle [] t = t
-- runTurtle (move:moves) t = runTurtle moves $ move t

-- TASK 2 =================================================

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Eq, Show)
testTree = Node 1 (Node 2 Leaf Leaf) (Node 3 (Node 4 Leaf Leaf) Leaf)


treeFilter :: (a -> Bool) -> Tree a -> Tree a
treeFilter _ Leaf = Leaf
treeFilter f (Node n l r)
  | f n       = Node n (treeFilter f l) (treeFilter f r)
  | otherwise = Leaf

levelMap :: (Int -> a -> b) -> Tree a -> Tree b
levelMap f t = levelMap' f t 0
  where
    levelMap' :: (Int -> a -> b) -> Tree a -> Int -> Tree b
    levelMap' _ Leaf _ = Leaf
    levelMap' f (Node n l r) lvl = Node (f lvl n)
                                        (levelMap' f l (lvl+1))
                                        (levelMap' f r (lvl+1))

isSubtree :: Eq a => Tree a -> Tree a -> Bool
isSubtree Leaf _ = True
isSubtree _ Leaf = False
isSubtree t@(Node n l r) (Node n2 l2 r2)
  | n /= n2   = isSubtree t l2 || isSubtree t r2
  | otherwise = testRest l l2 && testRest r r2
                || isSubtree t l2
                || isSubtree t r2
    where
      testRest :: Eq a => Tree a -> Tree a -> Bool
      testRest Leaf Leaf = True
      testRest Leaf _ = False
      testRest _ Leaf = False
      testRest t@(Node n l r) (Node n2 l2 r2)
        | n /= n2   = False
        | otherwise = testRest l l2 && testRest r r2


-- TASK 3 =================================================

data Pred = And Pred Pred | Or Pred Pred | Not Pred | Val Bool

expr = And (Or (Val True) (Not (Val True))) (Not (And (Val True) (Val False)))

eval :: Pred -> Bool
eval (And p b) = eval p && eval b
eval (Or p b)  = eval p || eval b
eval (Not p)   = not $ eval p
eval (Val b)   = b



-- TASK 4 =================================================

type TrackTitle = String
type TrackNo = Int
type AlbumName = String

data Track = Track TrackTitle TrackNo AlbumName

sortTracks :: [String] -> [String]
sortTracks [] = []
sortTracks xs = sortBy sortGT xs
  where
    sortGT s1 s2
      | (words s1 !! 1) < (words s2 !! 1) = LT
      | (words s1 !! 1) > (words s2 !! 1) = GT
      | otherwise                         = EQ

numberOfPlays :: [String] -> Integer
numberOfPlays = foldr ((+) . read . takeWhile (/= ' ')) 0


-- TASK 5 ======================================================

possibleRoutes :: String -> Int
possibleRoutes = possibles 1
  where
    possibles :: Int -> String -> Int
    possibles n [x] = n
    possibles n (x:y:s)
      |    (x == 'N' || x == 'S')
        && (y == 'E' || y == 'W')
        && null s                 = n*2
      |    (x == 'N' || x == 'S')
        && (y == 'E' || y == 'W') = possibles (n*2) s
      |     x == 'E' || x == 'W'
        ||  x == 'N' || x == 'S'  = possibles n (y:s)
      | otherwise                 = error "invalid input!"
