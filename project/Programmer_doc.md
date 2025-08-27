# Symbolic Differentiator in Haskell

This project implements a symbolic differentiator in Haskell. It defines an abstract syntax tree (AST) for mathematical expressions, functions for symbolic differentiation, simplification, pretty-printing, and numerical evaluation, along with unit tests.

## Architecture Overview

### Expression AST (`Expr`)

The central data type `Expr` encodes mathematical expressions:

```haskell
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
```

* `C Rational`: exact rational constants.
* `V Var`: variables (`Var = String`).
* Arithmetic operators: `Add`, `Mul`, `Div`, `Pow`.
* Unary operations: `Neg`, trigonometric, inverse trigonometric, exponential, logarithmic, hyperbolic functions.

### Pretty Printer

`pp :: Expr -> String` converts expressions into human-readable strings, with operator precedence and parentheses handled correctly.

### Smart Constructors

`sAdd` and `sMul` ensure addition and multiplication nodes remain flat, avoiding nested structures and removing degenerate cases (like empty lists).

### Evaluator

`eval :: M.Map Var Double -> Expr -> Double` evaluates an expression given an environment mapping variables to values.

### Differentiator

`derive :: Var -> Expr -> Expr` applies symbolic differentiation rules recursively:

* Constants → 0
* Variables → 1 if matching the differentiation variable, else 0
* Sum rule, product rule, quotient rule
* Power rules (constant exponent, general exponent)
* Chain rules for sin, cos, tan, exp, log, sinh, cosh, asin, acos, atan

### Simplifier

`simplify :: Expr -> Expr` applies algebraic simplification repeatedly until a fixpoint:

* Flatten nested adds/muls
* Remove neutral elements (0, 1)
* Fold constants
* Combine like terms
* Handle special power cases (`x^0 = 1`, `x^1 = x`, preserve `0^0`)

Helper functions:

* `splitConsts` separates constants from symbolic parts.
* `combineLike` merges terms with identical bases and sums their coefficients.
* `orderExpr` enforces a canonical ordering.

### Testing

Three sets of unit tests:

1. **Simplifier tests**: check flattening, constant folding, neutral element removal, like-term combination.
2. **Symbolic derivator tests**: compare derived expressions with expected symbolic results.
3. **Numeric derivator tests**: compare symbolic derivative evaluation against finite-difference approximations.

### Finite Differences

`finiteDiff :: M.Map Var Double -> Expr -> Double` estimates numerical derivatives using central difference approximation for validation.

## Example

```haskell
let expr = Mul [V "x", V "x"]
pp expr                -- "x*x"
derive "x" expr        -- symbolic derivative tree
pp (derive "x" expr)   -- "(2*x)"
simplify (derive "x" expr) -- simplified to Mul [C 2, V "x"]
eval (M.fromList [("x",2)]) (simplify (derive "x" expr)) -- 4.0
```

## File Layout

* **AST & Smart Constructors**: Expression definition, `sAdd`, `sMul`
* **Pretty Printer**: `pp`
* **Evaluator**: `eval`
* **Differentiator**: `derive`
* **Simplifier**: `simplify`, helpers
* **Tests**: `testSimplifier`, `testDerivatorSymbolic`, `testDerivatorNumeric`
* **Main**: runs all tests and prints results

## Limitations

* No handling of domain restrictions (e.g., log of negative numbers).
* No simplification across different algebraic forms beyond like-term combination.
* `0^0` is preserved rather than resolved.

## Future Work

* Extend simplification with distributive laws.
* Support additional functions (e.g., tanh, sec).
* Enhance canonicalization of expressions.
* Implement LaTeX pretty-printing for better readability.

---

This project demonstrates how symbolic algebra systems can be structured in Haskell using algebraic data types, recursive functions, and functional programming idioms.
