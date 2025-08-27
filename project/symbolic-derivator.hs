{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Map as M
import Data.Ratio ((%))
import Data.List (intersperse, sortBy, groupBy)
import Data.Function (on)

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
  Add es -> simplifyAdd (map simplify es)
  Mul es -> simplifyMul (map simplify es)
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

-- Split out all rationals
splitConsts :: [Expr] -> ([Rational],[Expr])
splitConsts = foldr f ([],[])
  where
    f (C r) (cs,rs) = (r:cs, rs)
    f x (cs,rs)     = (cs, x:rs)

-- Group like terms and sum coefficients
combineLike :: [Expr] -> [Expr]
combineLike xs =
  let keyed  = map splitCoeff xs
      groups = groupBy ((==) `on` snd) (sortBy (compare `on` snd) keyed)
      built  = [ build (map fst g) (snd (head g)) | g <- groups ]
  in filter (/= C 0) built
  where
    splitCoeff (Mul (C c:ys)) = (c, sMul ys)
    splitCoeff (C c)          = (c, C 1)
    splitCoeff e              = (1, e)
    build cs e = case sum cs of
                   0 -> C 0
                   1 -> e
                   n -> sMul [C n, e]

simplifyAdd :: [Expr] -> Expr
simplifyAdd es =
  let flat = concatMap (\case Add xs -> xs; x -> [x]) es
      simp = filter (/= C 0) flat
      (consts, rest) = splitConsts simp
      grouped = combineLike rest
      csum = sum consts
      result = (if csum /= 0 then [C csum] else []) ++ grouped
  in case result of
      []  -> C 0
      [x] -> x
      xs  -> Add (sortBy orderExpr xs)

simplifyMul :: [Expr] -> Expr
simplifyMul es =
  let flat = concatMap (\case Mul xs -> xs; x -> [x]) es
      simp = filter (/= C 1) flat
      (consts, rest) = splitConsts simp
      cprod = product consts
  in if any (==0) consts then C 0 else
       case (cprod, rest) of
        (1,[]) -> C 1
        (1,[x]) -> x
        (1,xs) -> Mul (sortBy orderExpr xs)
        (_,[]) -> C cprod
        (_,xs) -> Mul (sortBy orderExpr (C cprod:xs))

-- Constants first, then variables, then others
orderExpr :: Expr -> Expr -> Ordering
orderExpr (C _) (C _) = EQ
orderExpr (C _) _     = LT
orderExpr _ (C _)     = GT
orderExpr (V a) (V b) = compare a b
orderExpr (V _) _     = LT
orderExpr _ (V _)     = GT
orderExpr a b         = compare a b

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


-- Unit tests for simplifier
testSimplifier :: IO ()
testSimplifier = do
  let t name expr expected =
        let got = simplify expr
        in if got == expected
           then putStrLn $ "[OK] " ++ name ++
                "\n  expr:      " ++ pp expr ++
                "\n  expected:  " ++ pp expected ++
                "\n  actual:    " ++ pp got ++ "\n"
           else putStrLn $ "[FAIL] " ++ name ++
                "\n  expr:      " ++ pp expr ++
                "\n  expected:  " ++ pp expected ++
                "\n  actual:    " ++ pp got ++ "\n"

  -- Test addition flattening and constant folding
  t "Add flatten" (Add [C 1, Add [C 2, V "x"]]) (Add [C 3, V "x"])
  t "Mul flatten" (Mul [C 2, Mul [C 3, V "x"]]) (Mul [C 6, V "x"])
  t "Zero add"    (Add [C 0, V "x"]) (V "x")
  t "Zero mul"    (Mul [C 0, V "x"]) (C 0)
  t "One mul"     (Mul [C 1, V "x"]) (V "x")
  t "Combine like" (Add [Mul [C 2, V "x"], Mul [C 3, V "x"]]) (Mul [C 5, V "x"])
  t "Mul constants" (Mul [C 2, C 3, V "x"]) (Mul [C 6, V "x"])
  t "Add constants" (Add [C 2, C 3, V "x"]) (Add [C 5, V "x"])
  t "Neg Neg" (Neg (Neg (V "x"))) (V "x")
  t "Pow 0" (Pow (V "x") (C 0)) (C 1)
  t "Pow 1" (Pow (V "x") (C 1)) (V "x")

-- main function for testing
main :: IO ()
main = do
  testSimplifier
  let x = V "x"
      env = M.fromList [("x", 2.0)]
  putStrLn ("d/dx x = " ++ pp (derive "x" x))
  let f = Mul [x,x]
  putStrLn ("f(x)   = " ++ pp f)
  let df = simplify (derive "x" f)
  putStrLn ("f'(x)  = " ++ pp df)
  putStrLn ("df(2) ~= " ++ show (eval env df) ++ " vs FD " ++ show (finiteDiff env f))


