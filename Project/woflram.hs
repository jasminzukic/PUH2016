import Data.List
import Data.Char

data Assoc = LeftAs | RightAs deriving Eq

data Expression a = Null | Node a (Expression a) (Expression a)

instance (Show a) => Show (Expression a) where
  show Null         = ""
  show (Node x y z) = "(" ++ show y ++ " " ++ show x ++ " " ++ show z ++ ")"

isOperator :: String -> Bool
isOperator x = x `elem` ["+","-","*","/","^"]

isFunction :: String -> Bool
isFunction x = x `elem` ["sin", "cos", "exp", "log"]

associativityOf :: String -> Assoc
associativityOf x
  | x `elem` ["^"] = RightAs
  | otherwise    = LeftAs

precedenceOf :: String -> Int
precedenceOf x
  | x `elem` ["+","-"]     = 2
  | x `elem` ["*","/","%"] = 3
  | x `elem` ["^","!"]     = 4
  | otherwise              = 0

infixToRPN :: [String] -> [String] -> [String]-> [String]
infixToRPN [] [] ys        = ys
infixToRPN [] (s:stack) ys = infixToRPN [] stack (s:ys)
infixToRPN (x:xs) stack ys
  | isOperator x = if not (null stack)
                   && (assocX == LeftAs && precX <= precedenceOf (head stack)
                      || assocX == RightAs && precX < precedenceOf (head stack))
                   then infixToRPN (x:xs) (tail stack) (head stack:ys)
                   else infixToRPN xs (x:stack) ys
  | isFunction x = infixToRPN xs (x:stack) ys
  | x == "("     = infixToRPN xs (x:stack) ys
  | x == ")"     = if head stack /= "("
                   then infixToRPN (x:xs) (tail stack) (head stack:ys)
                   else infixToRPN xs (tail stack) ys
  | otherwise    = infixToRPN xs stack (x:ys)
  where
    assocX = associativityOf x
    precX = precedenceOf x

expression = "2 * x + 3 * y ^ 2 + cos y"

createRPN :: String -> [String]
createRPN s = reverse $ infixToRPN (words s) [] []

createExpression :: String -> Expression String
createExpression s = createExp (createRPN s) []
  where
    createExp :: [String] -> [Expression String] -> Expression String
    createExp [] [s] = s
    createExp (x:xs) stack
      | isOperator x = createExp xs $ opNode:tail (tail stack)
      | isFunction x = createExp xs $ funNode:tail stack
      | otherwise    = createExp xs $ leafNode:stack
      where
        opNode = Node x (head $ tail stack) (head stack)
        funNode = Node x Null (head stack)
        leafNode = Node x Null Null


substitute :: String -> Expression String -> Int -> Expression String
substitute _ Null _ = Null
substitute var (Node x y z) val
  | var == x  = Node (show val) (substitute var y val) (substitute var z val)
  | otherwise = Node x (substitute var y val) (substitute var z val)
