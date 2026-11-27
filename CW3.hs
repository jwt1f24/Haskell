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


-- ASSESSMENT 3
-- R1
eval1 :: Configuration -> Configuration
eval1 (Var x, env, k) =
 case lookup x env of
  Just v -> (v, env, k)
  Nothing -> error "eval1 terminated, undefined variable."

-- R2
eval1 (Lam x body, env, k) = (Closure x body env, env, k)

-- R3
eval1 (App e1 e2, env, k) = (e1, env, Arg e2 env : k)

-- R4
eval1 (cls@(Closure _ _ _), env, Arg e2 env' : k) = (e2, env', ClosureFrame cls : k)

-- R5
eval1 (v, env, ClosureFrame (Closure x body cenv) : k) = (body, (x, v) : cenv, k)

-- catch for invalid configurations
eval1 (_, _, _) = error "eval1 terminated, invalid configuration"
