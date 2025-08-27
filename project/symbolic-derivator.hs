{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Map as M

type Var = String

-- expression data type
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

-- smart constructors to simplify add and mul expressions
sAdd :: [Expr] -> Expr
sAdd xs = case concatMap unwrap xs of
  []  -> C 0
  [y] -> y
  ys  -> Add ys
  where unwrap (Add ys) = ys
        unwrap y        = [y]

sMul :: [Expr] -> Expr
sMul xs = case concatMap unwrap xs of
  []  -> C 1
  [y] -> y
  ys  -> Mul ys
  where unwrap (Mul ys) = ys
        unwrap y        = [y]

-- pretty printer
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

-- helper function to join strings with a separator
joinWith :: String -> [String] -> String
joinWith _ []     = ""
joinWith _ [x]    = x
joinWith s (x:xs) = x ++ s ++ joinWith s xs

-- evaluator
eval :: M.Map Var Double -> Expr -> Double
eval env = \case
  C r     -> fromRational r
  V v     -> env M.! v
  Neg e   -> negate (eval env e)
  Add es  -> sum (map (eval env) es)
  Mul es  -> product (map (eval env) es)
  Div a b -> eval env a / eval env b
  Pow a b -> eval env a ** eval env b
  Sin e   -> sin (eval env e)
  Cos e   -> cos (eval env e)
  Tan e   -> tan (eval env e)
  Asin e  -> asin (eval env e)
  Acos e  -> acos (eval env e)
  Atan e  -> atan (eval env e)
  Exp e   -> exp (eval env e)
  Log e   -> log (eval env e)
  Sinh e  -> sinh (eval env e)
  Cosh e  -> cosh (eval env e)

-- main function for testing
main :: IO ()
main = do
  putStrLn $ "Expected: (x+1.0)"
  putStrLn $ "Actual:   " ++ pp (Add [V "x", C 1])
  putStrLn $ "Expected: (1.0+2.0+y)"
  putStrLn $ "Actual:   " ++ pp (sAdd [C 1, Add [C 2, V "y"]])
  let env = M.fromList [("x", 2), ("y", 3)]
  putStrLn $ "Expected: 3.0"
  putStrLn $ "Actual:   " ++ show (eval env (Add [V "x", C 1]))
  putStrLn $ "Expected: 6.0"
  putStrLn $ "Actual:   " ++ show (eval env (sAdd [C 1, Add [C 2, V "y"]]))