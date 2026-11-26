import Prelude hiding (lookup)

-- ASSESSMENT 1
-- type synonym for environment
type Environment = [(String, Expr)]

-- modified data type for lambda calculus abstract syntax trees
data Expr = Var String
            | Lam String Expr
            | App Expr Expr
            | Closure String Expr Environment
            deriving (Eq, Show, Read)

-- lookup function
lookup :: String -> Environment -> Maybe Expr
lookup _ [] = Nothing
lookup str ((x, y) : rest)
 | str == x = Just y
 | otherwise = lookup str rest

-- test environments
env1 :: Environment
env1 = [("x", Var "a"), ("y", Var "b"), ("x", Var "c")]

env2 :: Environment
env2 = [("a", Closure "b" (Var "b") [])]


-- ASSESSMENT 2
-- data type
data Frame = Arg Expr Environment
             | ClosureFrame Expr
             deriving (Eq, Show, Read)

-- type synonym
type Continuation = [Frame]
type Configuration = (Expr, Environment, Continuation)
