{-# LANGUAGE LambdaCase #-}
module Main where

type Var = String

data Expr
  = C Rational
  | V Var
  | Neg Expr
  | Add [Expr]
  | Mul [Expr]
  | Div Expr Expr
  | Pow Expr Expr
  | Sin Expr | Cos Expr | Tan Expr
  | Asin Expr | Acos Expr | Atan Expr
  | Exp Expr | Log Expr
  | Sinh Expr | Cosh Expr
  deriving (Eq, Ord, Show)

pp :: Expr -> String
pp = \case
  C r      -> show (fromRational r :: Double)
  V v      -> v
  Neg e    -> "-" ++ pp e
  Add es   -> "(" ++ joinWith "+" (map pp es) ++ ")"
  Mul es   -> "(" ++ joinWith "*" (map pp es) ++ ")"
  Div a b  -> "(" ++ pp a ++ "/" ++ pp b ++ ")"
  Pow a b  -> "(" ++ pp a ++ "^" ++ pp b ++ ")"
  Sin e    -> "sin("  ++ pp e ++ ")"
  Cos e    -> "cos("  ++ pp e ++ ")"
  Tan e    -> "tan("  ++ pp e ++ ")"
  Asin e   -> "asin(" ++ pp e ++ ")"
  Acos e   -> "acos(" ++ pp e ++ ")"
  Atan e   -> "atan(" ++ pp e ++ ")"
  Exp e    -> "exp("  ++ pp e ++ ")"
  Log e    -> "log("  ++ pp e ++ ")"
  Sinh e   -> "sinh(" ++ pp e ++ ")"
  Cosh e   -> "cosh(" ++ pp e ++ ")"

joinWith :: String -> [String] -> String
joinWith _ []     = ""
joinWith _ [x]    = x
joinWith s (x:xs) = x ++ s ++ joinWith s xs

main :: IO ()
main = putStrLn (pp (Add [V "x", C 1]))