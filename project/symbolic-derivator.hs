{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Map as M
import Data.Ratio ((%))

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
pp = go 0
  where
    go :: Int -> Expr -> String
    go prec = \case
      C r      -> show (fromRational r :: Double)
      V v      -> v
      Neg e    -> "-" ++ go 4 e
      Add es   -> parens (prec>1) (joinWith "+" (map (go 1) es))
      Mul es   -> parens (prec>2) (joinWith "*" (map (go 2) es))
      Div a b  -> parens (prec>2) (go 2 a ++ "/" ++ go 2 b)
      Pow a b  -> parens (prec>3) (go 3 a ++ "^" ++ go 4 b)
      Sin e    -> "sin("  ++ go 0 e ++ ")"
      Cos e    -> "cos("  ++ go 0 e ++ ")"
      Tan e    -> "tan("  ++ go 0 e ++ ")"
      Asin e   -> "asin(" ++ go 0 e ++ ")"
      Acos e   -> "acos(" ++ go 0 e ++ ")"
      Atan e   -> "atan(" ++ go 0 e ++ ")"
      Exp e    -> "exp("  ++ go 0 e ++ ")"
      Log e    -> "log("  ++ go 0 e ++ ")"
      Sinh e   -> "sinh(" ++ go 0 e ++ ")"
      Cosh e   -> "cosh(" ++ go 0 e ++ ")"

joinWith :: String -> [String] -> String
joinWith _ []     = ""
joinWith _ [x]    = x
joinWith s (x:xs) = x ++ s ++ joinWith s xs

parens :: Bool -> String -> String
parens True s  = "("++s++")"
parens False s = s

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

-- derivator
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
  Pow f g -> sMul [ Pow f g
                  , sAdd [ sMul [derive x g, Log f]
                         , Div (sMul [g, derive x f]) f ] ]
  Sin e   -> sMul [Cos e, derive x e]
  Cos e   -> Neg (sMul [Sin e, derive x e])
  Tan e   -> sMul [sAdd [C 1, Pow (Tan e) (C 2)], derive x e]
  Exp e   -> sMul [Exp e, derive x e]
  Log e   -> Div (derive x e) e
  Sinh e  -> sMul [Cosh e, derive x e]
  Cosh e  -> sMul [Sinh e, derive x e]
  Asin e  -> Div (derive x e)
                  (Pow (sAdd [C 1, Neg (Pow e (C 2))]) (C (1%2)))
  Acos e  -> Neg (Div (derive x e)
                  (Pow (sAdd [C 1, Neg (Pow e (C 2))]) (C (1%2))))
  Atan e  -> Div (derive x e) (sAdd [C 1, Pow e (C 2)])

replace :: Int -> a -> [a] -> [a]
replace i x xs = let (l,_:r) = splitAt i xs in l ++ x:r


-- simplifier
simplify :: Expr -> Expr
simplify = fixpoint step
  where
    fixpoint f e = let e' = f e in if e' == e then e else fixpoint f e'

step :: Expr -> Expr
step = \case
  Add es -> sAdd (map simplify es)
  Mul es -> sMul (map simplify es)
  Neg (Neg e) -> simplify e
  Neg e -> Neg (simplify e)
  Div a b -> case (simplify a, simplify b) of
    (C 0, _) -> C 0
    (x, C 1) -> x
    (x,y)    -> Div x y
  Pow b (C 0) -> C 1
  Pow b (C 1) -> simplify b
  Pow b e     -> Pow (simplify b) (simplify e)
  other -> other


-- approximate equality for testing
approx :: Double -> Double -> Bool
approx a b = abs (a-b) <= 1e-6 * (1 + max (abs a) (abs b))

finiteDiff :: M.Map Var Double -> Expr -> Double
finiteDiff env f =
  let h  = 1e-6
      x0 = env M.! "x"
      envL = M.insert "x" (x0 - h) env
      envR = M.insert "x" (x0 + h) env
  in (eval envR f - eval envL f) / (2*h)


-- main function for testing
main :: IO ()
main = do
  let x = V "x"
      env = M.fromList [("x", 2.0)]
  putStrLn ("d/dx x = " ++ pp (derive "x" x))
  let f = Mul [x,x]
  putStrLn ("f(x)   = " ++ pp f)
  let df = simplify (derive "x" f)
  putStrLn ("f'(x)  = " ++ pp df)
  putStrLn ("df(2) ~= " ++ show (eval env df) ++ " vs FD " ++ show (finiteDiff env f))


