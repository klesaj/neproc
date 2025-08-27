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

derive :: Var -> Expr -> Expr
derive x = \case
  C _     -> C 0
  V y     -> C (if x == y then 1 else 0)
  Neg e   -> Neg (derive x e)
  Add es  -> sAdd (map (derive x) es)
  Mul es  -> sAdd [ sMul (replace i (derive x ei) es)
                  | (i,ei) <- zip [0..] es]
  Div f g -> Div (sAdd [ sMul [derive x f, g]
                       , Neg (sMul [f, derive x g])])
                  (Pow g (C 2))
  Pow f (C n) -> sMul [ C n, Pow f (sAdd [C n, C (-1)]), derive x f]
  e -> error ("derive: unsupported " ++ show e)

replace :: Int -> a -> [a] -> [a]
replace i x xs = let (l,_:r) = splitAt i xs in l ++ x:r

-- main function for testing
main :: IO ()
main = do
  putStrLn $ "Derivative of x w.r.t x"
  putStrLn $ "Expected: 1.0"
  putStrLn $ "Actual:   " ++ pp (derive "x" (V "x"))
  putStrLn $ ""

  putStrLn $ "Derivative of y w.r.t x"
  putStrLn $ "Expected: 0.0"
  putStrLn $ "Actual:   " ++ pp (derive "x" (V "y"))
  putStrLn $ ""

  putStrLn $ "Derivative of (x+1) w.r.t x"
  putStrLn $ "Expected: 1.0"
  putStrLn $ "Actual:   " ++ pp (derive "x" (Add [V "x", C 1]))
  putStrLn $ ""

  putStrLn $ "Derivative of (x*y) w.r.t x"
  putStrLn $ "Expected: (1.0*y+x*0.0)"
  putStrLn $ "Actual:   " ++ pp (derive "x" (Mul [V "x", V "y"]))
  putStrLn $ ""

  putStrLn $ "Derivative of (x/y) w.r.t x"
  putStrLn $ "Expected: ((1.0*y-(x*0.0))/(y^2.0))"
  putStrLn $ "Actual:   " ++ pp (derive "x" (Div (V "x") (V "y")))
  putStrLn $ ""

  putStrLn $ "Derivative of (x^3) w.r.t x"
  putStrLn $ "Expected: (3.0*(x^(3.0+(-1.0)))*1.0)"
  putStrLn $ "Actual:   " ++ pp (derive "x" (Pow (V "x") (C 3)))