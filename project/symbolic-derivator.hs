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
  deriving (Eq, Ord, Show)

main :: IO ()
main = putStrLn "WIP"