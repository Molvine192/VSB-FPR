data Expr = Num Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Var Char
          deriving (Eq)

instance (Show Expr) where
    show = showExpr

eval :: Expr -> Int
eval (Num x) = x
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Div a b) = div (eval a) (eval b)

showExpr :: Expr -> String
showExpr (Num x) = show x
showExpr (Var x) = [x]
showExpr (Add a b) = "(" ++ showExpr a ++ "+" ++ showExpr b ++ ")"
showExpr (Sub a b) = "(" ++ showExpr a ++ "-" ++ showExpr b ++ ")"
showExpr (Mul a b) = showExpr a ++ "*" ++ showExpr b
showExpr (Div a b) = showExpr a ++ "/" ++ showExpr b

deriv :: Expr -> Char -> Expr
deriv (Num _) dx = Num 0
deriv (Var x) dx = if x == dx then Num 1 else Num 0
deriv (Add a b) dx = Add (deriv a dx) (deriv b dx)
deriv (Sub a b) dx = Sub (deriv a dx) (deriv b dx)
deriv (Mul a b) dx = Add (Mul (deriv a dx) b) (Mul a (deriv b dx))
deriv (Div a b) dx = Div (Add (Mul (deriv a dx) b) (Mul a (deriv b dx))) (Mul b b)