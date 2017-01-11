import Data.List
import Data.Char

data Assoc = LeftAs | RightAs deriving Eq

data Expression = Var String
                | Const Double
                | Fun String Expression
                | Op String Expression Expression
                deriving (Eq)


instance Show Expression where
  show (Const x)
    | isInt x   = show (fromInteger (round x)) ++ " "
    | otherwise = show x ++ " "
  show (Var x) = x ++ " "
  show (Fun x fun@(Fun _ _)) = x ++ " ( " ++ show fun ++ ") "
  show (Fun x op@Op{}) = x ++ " ( " ++ show op ++ ") "
  show (Fun x y) = x ++ " " ++ show y
  show (Op x frst@(Op y _ _) scnd@(Op z _ _))
    | prec x > prec y && prec x > prec z  = "( " ++ show frst ++ ") " ++ x ++ " " ++ "( " ++ show scnd ++ ") "
    | prec x <= prec y && prec x > prec z = show frst ++ x ++ " " ++ "( " ++ show scnd ++ ") "
    | prec x > prec y && prec x <= prec z = "( " ++ show frst ++ ") " ++ x ++ " " ++ show scnd
    | otherwise                           = show frst ++ x ++ " " ++ show scnd
  show (Op x frst@(Op y _ _) scnd)
    | prec x > prec y = "( " ++ show frst ++ ") " ++ x ++ " " ++ show scnd
    | otherwise       = show frst ++ x ++ " " ++ show scnd
  show (Op x frst scnd@(Op y _ _))
    | prec x > prec y = show frst ++ x ++ " ( " ++ show scnd ++ ") "
    | otherwise       = show frst ++ x ++ " " ++ show scnd
  show (Op x y z) = show y ++ x ++ " " ++ show z

isInt :: Double -> Bool
isInt x = x == fromInteger (round x)

isOperator :: String -> Bool
isOperator x = x `elem` ["+","-","*","/","^"]

isFunction :: String -> Bool
isFunction x = x `elem` ["sin", "cos", "exp", "log"]

assoc :: String -> Assoc
assoc x
  | x `elem` ["^"] = RightAs
  | otherwise    = LeftAs

prec :: String -> Int
prec x
  | x `elem` ["+","-"] = 2
  | x `elem` ["*","/"] = 3
  | x `elem` ["^"]     = 4
  | otherwise          = 0

infixToRPN :: [String] -> [String] -> [String]-> [String]
infixToRPN [] [] ys        = ys
infixToRPN [] (s:stack) ys = infixToRPN [] stack (s:ys)
infixToRPN (x:xs) stack ys
  | isOperator x = if not (null stack)
                   && (assoc x == LeftAs && prec x <= prec (head stack)
                      || assoc x == RightAs && prec x < prec (head stack))
                   then infixToRPN (x:xs) (tail stack) (head stack:ys)
                   else infixToRPN xs (x:stack) ys
  | isFunction x = infixToRPN xs (x:stack) ys
  | x == "("     = infixToRPN xs (x:stack) ys
  | x == ")"     = if head stack /= "("
                   then infixToRPN (x:xs) (tail stack) (head stack:ys)
                   else infixToRPN xs (tail stack) ys
  | otherwise    = infixToRPN xs stack (x:ys)

expression = createExpression "2 * x + 3 * y ^ 2 + cos ( y + x )"

createRPN :: String -> [String]
createRPN s = reverse $ infixToRPN (words s) [] []

createExpression :: String -> Expression
createExpression s = createExp (createRPN s) []
  where
    createExp :: [String] -> [Expression] -> Expression
    createExp [] [s] = s
    createExp (x:xs) stack
      | isOperator x      = createExp xs $ opNode:tail (tail stack)
      | isFunction x      = createExp xs $ funNode:tail stack
      | all isDigit x     = createExp xs $ constNode:stack
      | (head x == '-') && all isDigit (tail x) = createExp xs $ constNode:stack
      | otherwise         = createExp xs $ varNode:stack
      where
        opNode = Op x (head $ tail stack) (head stack)
        funNode = Fun x (head stack)
        constNode = Const (read x :: Double)
        varNode = Var x


substitute :: String -> Expression -> Double -> Expression
substitute var expr val = fullSimplify $ substitute' var expr val
  where
    substitute' :: String -> Expression -> Double -> Expression
    substitute' var (Var x) val
      | var == x  = Const val
      | otherwise = Var x
    substitute' var (Const x) val = Const x
    substitute' var (Fun x y) val   = Fun x $ substitute' var y val
    substitute' var (Op x y z) val = Op x (substitute' var y val) (substitute' var z val)

evaluate :: Expression -> Double
evaluate (Var x) = error "Tree still contains variables. Substitute them"
evaluate (Const x) = x
evaluate (Fun x y)
  | x == "sin" = sin(evaluate y)
  | x == "cos" = cos(evaluate y)
  | x == "log" = log(evaluate y)
  | x == "exp" = exp(evaluate y)
  | otherwise  = error "Function not implemented"
evaluate (Op x y z)
  | x == "+"   = evaluate y + evaluate z
  | x == "-"   = evaluate y - evaluate z
  | x == "*"   = evaluate y * evaluate z
  | x == "/"   = evaluate y / evaluate z
  | x == "^"   = evaluate y ** evaluate z
  | otherwise = error "Operator not implemented"


simplify :: Expression -> Expression
simplify (Fun a b) = Fun a (simplify b)

simplify (Op "+" (Const a) (Const b)) = Const (a + b)
simplify (Op "+" a (Const 0))         = simplify a
simplify (Op "+" (Const 0) a)         = simplify a
simplify (Op "+" a (Op "-" b c))      = Op "-" (simplify (Op "+" (simplify a) (simplify b))) (simplify c)

simplify (Op "*" (Const (-1)) a)      = Op "-" (Const 0) (simplify a)

simplify (Op "-" (Const a) (Const b)) = Const (a - b)
simplify (Op "-" a (Const 0))         = simplify a

--simplify (Op "-" a b)                 = Op "+" (simplify a) (Op "*" (Const (-1)) (simplify b))

simplify (Op "*" (Const a) (Const b)) = Const(a * b)
simplify (Op "*" a (Const 1))         = simplify a
simplify (Op "*" (Const 1) a)         = simplify a
simplify (Op "*" a (Const 0))         = Const 0
simplify (Op "*" (Const 0) a)         = Const 0

simplify (Op "^" (Const a) (Const b)) = Const (a ** b)
simplify (Op "^" a (Const 1))         = simplify a
simplify (Op "^" a (Const 0))         = Const 1

simplify (Op "^" (Op "^" a (Const b)) (Const c)) = Op "^" a (Const (b * c))
simplify (Op "*" (Const a) (Op "*" (Const b) c)) = Op "*" (Const (a * b)) (simplify c)
simplify (Op "*" (Const a) (Op "*" c (Const b))) = Op "*" (Const (a * b)) (simplify c)
simplify (Op "*" c (Op "*" (Const a) (Const b))) = Op "*" (Const (a * b)) (simplify c)

simplify (Op "*" (Const a) (Op "+" b c)) = Op "+" (Op "*" (Const a) (simplify b)) (Op "*" (Const a) (simplify c))

simplify (Op "/" (Const 0) a)         = Const 0
simplify (Op "/" (Const a) (Const 0)) = error "division by zero"
simplify (Op "/" (Const a) (Const b)) | a == b = Const 1
simplify (Op "/" a (Const 1))         = simplify a

simplify (Op "/" a b) = Op "/" (simplify a) (simplify b)
simplify (Op "^" a b) = Op "^" (simplify a) (simplify b)
simplify (Op "*" a b) = Op "*" (simplify a) (simplify b)
simplify (Op "+" a b) = Op "+" (simplify a) (simplify b)
simplify x            = x

fullSimplify :: Expression -> Expression
fullSimplify expr = fullSimplify' expr (Const 0)
  where
    fullSimplify' :: Expression -> Expression -> Expression
    fullSimplify' cur last
      | cur == last = cur
      | otherwise   = let cur' = simplify cur
                      in fullSimplify' cur' cur

containsVariable :: String -> Expression -> Bool
containsVariable v (Const a) = False
containsVariable v (Var a)
  | v == a    = True
  | otherwise = False
containsVariable v (Fun a b) = containsVariable v b
containsVariable v (Op a b c) = containsVariable v b ||
                                containsVariable v c

deriveBy :: Expression -> String -> Expression
deriveBy (Const x) v = Const 0
deriveBy (Var a) v
  | v == a    = Const 1
  | otherwise = Const 0
deriveBy expr v
  | containsVariable v expr = fullSimplify (deriveBy' expr v)
  | otherwise               = Const 0
  where
    deriveBy' :: Expression -> String -> Expression
    deriveBy' (Fun a b) v
      | a == "sin" = Op "*" (deriveBy b v) (Fun "cos" b)
      | a == "cos" = Op "*" (deriveBy b v) (Op "*" (Const (-1)) (Fun "sin" b))
      | a == "exp" = Op "*" (deriveBy b v) (Fun "exp" b)
      | a == "log" = Op "*" (deriveBy b v) (Op "/" (Const 1) b)
    deriveBy' (Op "^" b (Const c)) v = Op "*" (Const c) (Op "^" b (Const (c-1)))
    deriveBy' (Op a b c) v
      | a == "+" = Op "+" (deriveBy b v) (deriveBy c v)
      | a == "*" = Op "+" (Op "*" b (deriveBy c v)) (Op "*" (deriveBy b v) c)
      | a == "-" = Op "-" (deriveBy b v) (deriveBy c v)
      | a == "/" = Op "/" (Op "-" (Op "*" (deriveBy b v) c) (Op "*" b (deriveBy c v))) (Op "^" c (Const 2))
