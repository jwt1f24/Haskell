import Prelude hiding (lookup)

-- ASSESSMENT 1
-- type synonym for environment
type Environment = [(String, Expr)]

-- modified data type for lambda calculus abstract syntax trees
data Expr = Var String
            | Lam String Expr
            | App Expr Expr
            | Closure String Expr Environment
            | Zero                            -- extension
            | Succ Expr                       -- extension
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
             | SuccFrame               -- modification
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

-- modified evaluation rules
eval1 (Succ e, env, k) = (e, env, SuccFrame : k)
eval1 (v, env, SuccFrame : k) = (Succ v, env, k)

-- catch for invalid configurations
eval1 (_, _, _) = error "eval1 terminated, invalid configuration"


-- ASSESSMENT 4
-- take lambda expression & evaluate using Call-by-Value until termination
eval :: Expr -> Expr
eval e = run (e, [], [])

-- return terminated expression once function terminates
run :: Configuration -> Expr
run (v, env, []) | isValue v = v
run cfg = run (eval1 cfg)


-- ASSESSMENT 5
-- test case 1, evaluate to closure lambda expression 
term1 :: Expr
term1 =
 App (Lam "v"
  (App
   (App (Var "v") (Lam "z" (Var
   "z"))) (App (Lam "v" (App
      (App (Var "v") (Lam "x" (Lam "y" (Var
      "x")))) (Lam "z" (Var "z")))
     )
     (Lam "x" (Lam "y" (Var "x")))
    )
   )
  )
 (Lam "x" (Lam "y" (Var "y")))

-- test case 2, apply new identity & return as a closure
term2 :: Expr
term2 = App (Lam "x" (Var "x")) (Lam "y" (Var "y"))

-- test case 3, unbound variable in empty environment
term3 :: Expr
term3 = Var "undefined_variable"

-- test case 4, handling termination values
term4 :: Expr
term4 = App (Lam "x" (Succ (Var "x"))) Zero


-- ASSESSMENT 6
-- handle termination values
isValue :: Expr -> Bool
isValue (Closure _ _ _) = True
isValue Zero = True
isValue (Succ e) = isValue e
isValue _ = False
