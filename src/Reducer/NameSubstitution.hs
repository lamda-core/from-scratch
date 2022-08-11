module Reducer.NameSubstitution where

import Core

evaluate :: Term -> Term
evaluate (App Err _) = Err
evaluate (App (Lam x (Var x')) b) | x == x' = evaluate b
evaluate (App (Lam x (App a1 a2)) b) = evaluate (App (evaluate (App (Lam x a1) b)) (evaluate (App (Lam x a2) b)))
evaluate (App (Lam x (Lam x' a)) _) | x == x' = Lam x (evaluate a)
evaluate (App (Lam x (Lam y a)) b) = Lam y (evaluate (App (Lam x a) b))
evaluate (App (Lam _ a) _) = evaluate a
evaluate (App (App (Call f) a) b) = case (f, evaluate a, evaluate b) of
  ("+", Int a, Int b) -> Int (a + b)
  ("-", Int a, Int b) -> Int (a - b)
  ("*", Int a, Int b) -> Int (a * b)
  ("==", Int a, Int b) -> Lam "T" (Lam "F" (Var (if a == b then "T" else "F")))
  (_, a, b) -> App (App (Call f) a) b
evaluate (App Fix a) = evaluate (App a (App Fix a))
evaluate (Lam x a) = Lam x (evaluate a)
evaluate a = a
