# Symbolic Differentiator in Haskell — User Documentation

## Overview

This program is a symbolic differentiator implemented in Haskell.
It provides a data type for mathematical expressions, supports evaluation, symbolic differentiation, simplification, and pretty-printing.
A set of built-in unit tests demonstrates correctness both symbolically and numerically.

---

## Expression Language

The core type is:

```haskell
data Expr
  = C Rational              -- Constant (e.g., C 2, C (1%2))
  | V String                -- Variable (e.g., V "x")
  | Neg Expr                -- Negation
  | Add [Expr]              -- Sum of terms
  | Mul [Expr]              -- Product of factors
  | Div Expr Expr           -- Quotient
  | Pow Expr Expr           -- Exponentiation
  | Sin Expr | Cos Expr | Tan Expr
  | Asin Expr | Acos Expr | Atan Expr
  | Exp Expr | Log Expr
  | Sinh Expr | Cosh Expr
```

### Examples

* $x^2 + 3$: `Add [Pow (V "x") (C 2), C 3]`
* $\sin(x) * e^x$: `Mul [Sin (V "x"), Exp (V "x")]`
* $\frac{x}{x+1}$: `Div (V "x") (Add [V "x", C 1])`

---

## Features

### 1. Evaluation

```haskell
eval :: M.Map String Double -> Expr -> Double
```

Evaluates an expression given a mapping from variable names to values.

Example:

```haskell
eval (M.fromList [("x", 2.0)]) (Pow (V "x") (C 2))
-- Result: 4.0
```

---

### 2. Pretty Printing

```haskell
pp :: Expr -> String
```

Converts expressions into human-readable form.

Example:

```haskell
pp (Add [Pow (V "x") (C 2), C 3])
-- "x^2+3.0"
```

---

### 3. Differentiation

```haskell
derive :: String -> Expr -> Expr
```

Computes the symbolic derivative of an expression with respect to a variable.

Example:

```haskell
pp (derive "x" (Pow (V "x") (C 2)))
-- "2.0*x"
```

Supported differentiation rules:

* Constants → 0
* Variables → 1 (if matches) else 0
* Sum rule, product rule, quotient rule
* Power rule (integer and general exponent)
* Chain rule for all functions: sin, cos, tan, exp, log, sinh, cosh, asin, acos, atan

---

### 4. Simplification

```haskell
simplify :: Expr -> Expr
```

Performs algebraic simplification:

* Flattens nested sums and products
* Removes neutral elements (`+0`, `*1`)
* Collapses constants
* Combines like terms (e.g., `2*x + 3*x → 5*x`)
* Handles `x^0 → 1`, `x^1 → x`, special case `0^0` is preserved

Example:

```haskell
pp (simplify (Add [V "x", V "x", C 3]))
-- "2.0*x+3.0"
```

---

## Testing

Three categories of tests are provided:

### 1. Simplifier Tests

Checks basic algebraic simplifications.
Example:
`Add [C 2, C 3, V "x"]` → `Add [C 5, V "x"]`

### 2. Symbolic Derivator Tests

Compares the output of `derive` to known symbolic results.
Example:
`d/dx (sin x)` → `cos x`

### 3. Numeric Derivator Tests

Compares symbolic derivatives to finite-difference approximations at numeric points.

---

## Running the Program

Compile:

```bash
ghc derivator.hs -o derivator
```

Run:

```bash
./derivator
```

You will see test results such as:

```
=== Simplifier tests ===
[OK] Add flatten
[OK] Mul flatten
...

=== Derivator symbolic tests ===
[OK] d/dx x
[OK] d/dx y
...

=== Derivator numeric tests ===
[OK] d/dx x^2
[OK] d/dx sin(x)
...
```

---

## Example Usage in `ghci`

```haskell
:l derivator.hs
let expr = Div (Sin (V "x")) (V "x")
pp expr
-- "sin(x)/x"

pp (simplify (derive "x" expr))
-- "(x*cos(x)-sin(x))/x^2"

eval (M.fromList [("x",1.0)]) expr
-- 0.8414709848078965
```

---

## Summary

The program provides:

* A symbolic expression AST
* Differentiation via recursive rules
* Algebraic simplification
* Pretty-printing and numeric evaluation
* Test suite for verification

It is extendable for more functions or further simplification rules if needed.
