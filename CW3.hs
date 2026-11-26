import Prelude hiding (lookup)

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
