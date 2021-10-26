module Test.PatternTests where

import Control.Monad.Free (Free)
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))
import Pattern (Error(..), Expr(..), eval, match)
import Prelude (Unit, discard, (#), ($))
import Result (Result(..))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

patternTests :: Free TestF Unit
patternTests = 
  suite "--== Pattern calculus ==--" do
    suite "☯︎ Match" do
      test "❌ x <- y  ∴  Undefined variable: x" do
        match (Var "x") (Var "y") Nil # Assert.equal (Err $ UndefinedVar "x")
      test "❌ _ <- y  ∴  Undefined variable: y" do
        match Any (Var "y") Nil # Assert.equal (Err $ UndefinedVar "y")
      test "❌ A <- B  ∴  A does not match B" do
        match (Ctr "A") (Ctr "B") Nil # Assert.equal (Err $ Ctr "A" `PatternMismatch` Ctr "B")
      test "❌ 1 <- 2  ∴  1 does not match 2" do
        match (Int 1) (Int 2) Nil # Assert.equal (Err $ Int 1 `PatternMismatch` Int 2)
      test "✅ _ <- A  ∴  Γ{}" do
        match Any (Ctr "A") Nil # Assert.equal (Ok Nil)
      test "✅ x <- A  Γ{x: x}  ∴  Γ{x: A}" do
        match (Var "x") (Ctr "A") ("x" `Tuple` Var "x" : Nil) # Assert.equal (Ok $ "x" `Tuple` Ctr "A" : Nil)
      test "✅ A <- y  Γ{y: y}  ∴  Γ{y: y}" do
        match (Ctr "A") (Var "y") ("y" `Tuple` Var "y" : Nil) # Assert.equal (Ok $ "y" `Tuple` Var "y" : Nil)
      test "✅ A <- A  ∴  Γ{}" do
        match (Ctr "A") (Ctr "A") Nil # Assert.equal (Ok Nil)
      test "❌ (A -> B) <- (C -> D)  ∴  Pattern mismatch: A ≠ C" do
        match (Ctr "A" `To` Ctr "B") (Ctr "C" `To` Ctr "D") Nil # Assert.equal (Err $ Ctr "A" `PatternMismatch` Ctr "C")
      test "❌ (A -> B) <- (A -> C)  ∴  Pattern mismatch: B ≠ C" do
        match (Ctr "A" `To` Ctr "B") (Ctr "A" `To` Ctr "C") Nil # Assert.equal (Err $ Ctr "B" `PatternMismatch` Ctr "C")
      test "✅ (A -> B) <- (A -> B)  ∴  Γ{}" do
        match (Ctr "A" `To` Ctr "B") (Ctr "A" `To` Ctr "B") Nil # Assert.equal (Ok Nil)
      test "❌ x | B <- A  ∴  Undefined variable: x" do
        match (Var "x" `Or` Ctr "B") (Ctr "A") Nil # Assert.equal (Err $ UndefinedVar "x")
      test "✅ A | B <- A  ∴  Γ{}" do
        match (Ctr "A" `Or` Ctr "B") (Ctr "A") Nil # Assert.equal (Ok Nil)
      test "✅ A | B <- B  ∴  Γ{}" do
        match (Ctr "A" `Or` Ctr "B") (Ctr "B") Nil # Assert.equal (Ok Nil)
      test "❌ A | B <- C  ∴  Pattern mismatch: B ≠ C" do
        match (Ctr "A" `Or` Ctr "B") (Ctr "C") Nil # Assert.equal (Err $ Ctr "B" `PatternMismatch` Ctr "C")
      test "❌ (A, B) <- (C, D)  ∴  Pattern mismatch: A ≠ C" do
        match (Ctr "A" `And` Ctr "B") (Ctr "C" `And` Ctr "D") Nil # Assert.equal (Err $ Ctr "A" `PatternMismatch` Ctr "C")
      test "❌ (A, B) <- (A, C)  ∴  Pattern mismatch: B ≠ C" do
        match (Ctr "A" `And` Ctr "B") (Ctr "A" `And` Ctr "C") Nil # Assert.equal (Err $ Ctr "B" `PatternMismatch` Ctr "C")
      test "✅ (A, B) <- (A, B)  ∴  Γ{}" do
        match (Ctr "A" `And` Ctr "B") (Ctr "A" `And` Ctr "B") Nil # Assert.equal (Ok Nil)
      test "❌ A B <- C D  ∴  Pattern mismatch: A ≠ C" do
        match (Ctr "A" `App` Ctr "B") (Ctr "C" `App` Ctr "D") Nil # Assert.equal (Err $ Ctr "A" `PatternMismatch` Ctr "C")
      test "❌ A B <- A C  ∴  Pattern mismatch: B ≠ C" do
        match (Ctr "A" `App` Ctr "B") (Ctr "A" `App` Ctr "C") Nil # Assert.equal (Err $ Ctr "B" `PatternMismatch` Ctr "C")
      test "✅ A B <- A B  ∴  Γ{}" do
        match (Ctr "A" `App` Ctr "B") (Ctr "A" `App` Ctr "B") Nil # Assert.equal (Ok Nil)

    suite "☯︎ Anything" do
      test "✅ _  ∴  _" do
        eval Any Nil # Assert.equal (Ok Any)

    suite "☯︎ Integer" do
      test "✅ 42  ∴  42" do
        eval (Int 42) Nil # Assert.equal (Ok $ Int 42)

    suite "☯︎ Constructor" do
      test "✅ A  ∴  A" do
        eval (Ctr "A") Nil # Assert.equal (Ok $ Ctr "A")

    suite "☯︎ Variable" do
      test "❌ x  ∴  Undefined variable: x" do
        eval (Var "x") Nil # Assert.equal (Err $ UndefinedVar "x")
      test "✅ x  Γ{x: x}  ∴  x" do
        eval (Var "x") ("x" `Tuple` Var "x" : Nil) # Assert.equal (Ok $ Var "x")
      test "✅ x  Γ{y: A, x: y}  ∴  A" do
        eval (Var "x") ("y" `Tuple` Ctr "A" : "x" `Tuple` Var "y" : Nil) # Assert.equal (Ok $ Ctr "A")

    suite "☯︎ To -- Pattern case" do
      test "❌ x -> y  ∴  Undefined variable: x" do
        eval (Var "x" `To` Var "y") Nil # Assert.equal (Err $ UndefinedVar "x")
      test "❌ A -> y  ∴  Undefined variable: y" do
        eval (Ctr "A" `To` Var "y") Nil # Assert.equal (Err $ UndefinedVar "y")
      test "✅ A -> B  ∴  A -> B" do
        eval (Ctr "A" `To` Ctr "B") Nil # Assert.equal (Ok $ Ctr "A" `To` Ctr "B")

    suite "☯︎ Or -- Union / Sum type / Disjunction" do
      test "❌ x | y  ∴  Undefined variable: x" do
        eval (Var "x" `Or` Var "y") Nil # Assert.equal (Err $ UndefinedVar "x")
      test "❌ A | y  ∴  Undefined variable: y" do
        eval (Ctr "A" `Or` Var "y") Nil # Assert.equal (Err $ UndefinedVar "y")
      test "✅ A | B  ∴  A | B" do
        eval (Ctr "A" `Or` Ctr "B") Nil # Assert.equal (Ok $ Ctr "A" `Or` Ctr "B")

    suite "☯︎ And -- Tuple / Product type / Conjunction" do
      test "❌ (x, y)  ∴  Undefined variable: x" do
        eval (Var "x" `And` Var "y") Nil # Assert.equal (Err $ UndefinedVar "x")
      test "❌ (A, y)  ∴  Undefined variable: y" do
        eval (Ctr "A" `And` Var "y") Nil # Assert.equal (Err $ UndefinedVar "y")
      test "✅ (A, B)  ∴  (A, B)" do
        eval (Ctr "A" `And` Ctr "B") Nil # Assert.equal (Ok $ Ctr "A" `And` Ctr "B")

    suite "☯︎ Application" do
      test "❌ x B  ∴  Undefined variable: x" do
        eval (Var "x" `App` Ctr "B") Nil # Assert.equal (Err $ UndefinedVar "x")
      test "❌ A y  ∴  Undefined variable: y" do
        eval (Ctr "A" `App` Var "y") Nil # Assert.equal (Err $ UndefinedVar "y")
      test "✅ _ B  ∴  _ B" do
        eval (Any `App` Ctr "B") Nil # Assert.equal (Ok $ Any `App` Ctr "B")
      test "✅ A B  ∴  A B" do
        eval (Ctr "A" `App` Ctr "B") Nil # Assert.equal (Ok $ Ctr "A" `App` Ctr "B")
      test "✅ x B  Γ{x: x}  ∴  x B" do
        eval (Var "x" `App` Ctr "B") ("x" `Tuple` Var "x" : Nil) # Assert.equal (Ok $ Var "x" `App` Ctr "B")
      test "✅ A B C  ∴  A B C" do
        eval (Ctr "A" `App` Ctr "B" `App` Ctr "C") Nil # Assert.equal (Ok $ Ctr "A" `App` Ctr "B" `App` Ctr "C")
      test "✅ (_ -> B) x  Γ{x: x}  ∴  B" do
        eval (Any `To` Ctr "B" `App` Var "x") ("x" `Tuple` Var "x" : Nil) # Assert.equal (Ok $ Ctr "B")
      test "✅ (x -> x) y  Γ{x: x, y: y}  ∴  y" do
        eval (Var "x" `To` Var "x" `App` Var "y") ("x" `Tuple` Var "x" : "y" `Tuple` Var "y" : Nil) # Assert.equal (Ok $ Var "y")
      test "✅ (A -> B) x  Γ{x: x}  ∴  (A -> A) x" do
        eval (Ctr "A" `To` Ctr "B" `App` Var "x") ("x" `Tuple` Var "x" : Nil) # Assert.equal (Ok $ (Ctr "A" `To` Ctr "B") `App` Var "x")
      test "❌ (A -> B) C  ∴  Pattern mismatch: A ≠ C" do
        eval (Ctr "A" `To` Ctr "B" `App` Ctr "C") Nil # Assert.equal (Err $ Ctr "A" `PatternMismatch` Ctr "C")
      test "✅ (A -> B) A  ∴  B" do
        eval (Ctr "A" `To` Ctr "B" `App` Ctr "A") Nil # Assert.equal (Ok $ Ctr "B")
      test "✅ (A -> C | B -> D) A  ∴  C" do
        eval ((Ctr "A" `To` Ctr "C") `Or` (Ctr "B" `To` Ctr "D") `App` Ctr "A") Nil # Assert.equal (Ok $ Ctr "C")
      test "✅ (A -> C | B -> D) B  ∴  C" do
        eval ((Ctr "A" `To` Ctr "C") `Or` (Ctr "B" `To` Ctr "D") `App` Ctr "B") Nil # Assert.equal (Ok $ Ctr "D")
      test "❌ (A -> C | B -> D) C  ∴  Pattern mismatch: B ≠ C" do
        eval ((Ctr "A" `To` Ctr "C") `Or` (Ctr "B" `To` Ctr "D") `App` Ctr "C") Nil # Assert.equal (Err $ Ctr "B" `PatternMismatch` Ctr "C")
      test "✅ (A, B) C  ∴  (A C, B C)" do
        eval ((Ctr "A" `And` Ctr "B") `App` Ctr "C") Nil # Assert.equal (Ok $ Ctr "A" `App` Ctr "C" `And` (Ctr "B" `App` Ctr "C"))

    suite "☯︎ Addition" do
      test "✅ (+)  ∴  (+)" do
        eval Add Nil # Assert.equal (Ok Add)
      test "✅ (+) 1 2  ∴  3" do
        eval (App (App Add (Int 1)) (Int 2)) Nil # Assert.equal (Ok $ Int 3)

    suite "☯︎ Subtraction" do
      test "✅ (-)  ∴  (-)" do
        eval Sub Nil # Assert.equal (Ok Sub)
      test "✅ (-) 2 1  ∴  1" do
        eval (App (App Sub (Int 2)) (Int 1)) Nil # Assert.equal (Ok $ Int 1)

    suite "☯︎ Multiplication" do
      test "✅ (*)  ∴  (*)" do
        eval Mul Nil # Assert.equal (Ok Mul)
      test "✅ (*) 2 3  ∴  6" do
        eval (App (App Mul (Int 2)) (Int 3)) Nil # Assert.equal (Ok $ Int 6)

    suite "☯︎ Equals" do
      test "✅ (==)  ∴  (==)" do
        eval Eq Nil # Assert.equal (Ok Eq)
      test "✅ (==) 2 2  ∴  1" do
        eval (App (App Eq (Int 2)) (Int 2)) Nil # Assert.equal (Ok $ Int 1)
      test "✅ (==) 2 3  ∴  0" do
        eval (App (App Eq (Int 2)) (Int 3)) Nil # Assert.equal (Ok $ Int 0)

    -- suite "☯︎ Recursive definition" do
    --   test "✅ x@1  ∴  1" do
    --     reduce (Rec "x" (Num 1.0)) Nil # Assert.equal (Ok (Num 1.0))
    --   test "✅ x@x  ∴  x@x" do
    --     reduce (Rec "x" $ Var "x") Nil # Assert.equal (Ok (Rec "x" $ Var "x"))
    --   test "✅ λy. x@x  ∴  x@(λy. x)" do
    --     reduce (Lam "y" $ Rec "x" $ Var "x") Nil # Assert.equal (Ok (Rec "x" $ Lam "y" $ Var "x"))
    --   test "✅ x@x y  Γ{y: y}  ∴  x@(x y)" do
    --     reduce (App (Rec "x" $ Var "x") (Var "y")) (Tuple "y" (Var "y") : Nil) # Assert.equal (Ok (Rec "x" $ App (Var "x") (Var "y")))
    --   test "✅ x y@y  Γ{x: x}  ∴  y@(x y)" do
    --     reduce (App (Var "x") (Rec "y" $ Var "y")) (Tuple "x" (Var "x") : Nil) # Assert.equal (Ok (Rec "y" $ App (Var "x") (Var "y")))
    --   test "✅ x@x x@x  ∴  x@(x x)" do
    --     reduce (App (Rec "x" $ Var "x") (Rec "x" $ Var "x")) Nil # Assert.equal (Ok (Rec "x" $ App (Var "x") (Var "x")))
    --   test "✅ x@x y@y  ∴  x@y@(x y)" do
    --     reduce (App (Rec "x" $ Var "x") (Rec "y" $ Var "y")) Nil # Assert.equal (Ok (Rec "x" $ Rec "y" $ App (Var "x") (Var "y")))
    --   test "✅ (x@λy. x) 1  ∴  (x@λy. x) 1" do
    --     reduce (App (Rec "x" $ Lam "y" $ Var "x") (Num 1.0)) Nil # Assert.equal (Ok (App (Rec "x" $ Lam "y" $ Var "x") (Num 1.0)))

    -- suite "☯︎ Factorial" do
    --   test "✅ f  Γ{f: factorial}  ∴  f@factorial" do
    --     reduce (Var "f") (Tuple "f" factorial : Nil) # Assert.equal (Ok (Rec "f" factorial))
    --   test "✅ f 0  Γ{f: factorial}  ∴  f@factorial 1" do
    --     reduce (App (Var "f") (Num 0.0)) (Tuple "f" factorial : Nil) # Assert.equal (Ok (App (Rec "f" factorial) (Num 0.0)))
    --   test "✅ f 0  Γ{f: factorial}  ∴  1" do
    --     eval (App (Var "f") (Num 0.0)) (Tuple "f" factorial : Nil) # Assert.equal (Ok (Num 1.0))
    --   test "✅ f 1  Γ{f: factorial}  ∴  1" do
    --     eval (App (Var "f") (Num 1.0)) (Tuple "f" factorial : Nil) # Assert.equal (Ok (Num 1.0))
    --   test "✅ f 2  Γ{f: factorial}  ∴  2" do
    --     eval (App (Var "f") (Num 2.0)) (Tuple "f" factorial : Nil) # Assert.equal (Ok (Num 2.0))
    --   test "✅ f 5  Γ{f: factorial}  ∴  120" do
    --     eval (App (Var "f") (Num 5.0)) (Tuple "f" factorial : Nil) # Assert.equal (Ok (Num 120.0))

    -- suite "☯︎ Ackermann" do
    --   test "✅ a  Γ{a: ackermann}  ∴  a@ackermann" do
    --     reduce (Var "a") (Tuple "a" ackermann : Nil) # Assert.equal (Ok (Rec "a" ackermann))
    --   test "✅ a 0  Γ{a: ackermann}  ∴  a@ackermann 0" do
    --     reduce (App (Var "a") (Num 0.0)) (Tuple "a" ackermann : Nil) # Assert.equal (Ok (App (Rec "a" ackermann) (Num 0.0)))
    --   test "✅ a 0  Γ{a: ackermann}  ∴  λn. n + 1" do
    --     eval (App (Var "a") (Num 0.0)) (Tuple "a" ackermann : Nil) # Assert.equal (Ok (Lam "n" $ add (Var "n") (Num 1.0)))
    --   test "✅ a 0 0  Γ{a: ackermann}  ∴  1" do
    --     eval (app2 (Var "a") (Num 0.0) (Num 0.0)) (Tuple "a" ackermann : Nil) # Assert.equal (Ok (Num 1.0))
    --   test "✅ a 1 1  Γ{a: ackermann}  ∴  3" do
    --     eval (app2 (Var "a") (Num 1.0) (Num 1.0)) (Tuple "a" ackermann : Nil) # Assert.equal (Ok (Num 3.0))
    --   test "✅ a 2 2  Γ{a: ackermann}  ∴  7" do
    --     eval (app2 (Var "a") (Num 2.0) (Num 2.0)) (Tuple "a" ackermann : Nil) # Assert.equal (Ok (Num 7.0))
    --   test "✅ a 3 2  Γ{a: ackermann}  ∴  29" do
    --     eval (app2 (Var "a") (Num 3.0) (Num 2.0)) (Tuple "a" ackermann : Nil) # Assert.equal (Ok (Num 29.0))

    -- where
    --   a = Var "a"
    --   f = Var "f"
    --   m = Var "m"
    --   n = Var "n"
    --   k0 = Num 0.0
    --   k1 = Num 1.0

    --   -- f 0 = 1
    --   -- f n = n * f (n - 1)
    --   factorial = Lam "n" $
    --     app2 (eq n k0) k1 (mul n (App f (sub n k1)))

    --   -- a 0 n = n + 1
    --   -- a m 0 = a (m-1) 1
    --   -- a m n = a (m-1) (a m (n-1))
    --   ackermann = Lam "m" $ Lam "n"
    --     (app2 (eq m k0) (add n k1)
    --     (app2 (eq n k0) (app2 a (sub m k1) k1)
    --     (app2 a (sub m k1) (app2 a m (sub n k1)))))
