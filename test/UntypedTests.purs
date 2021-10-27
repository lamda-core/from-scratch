module Test.UntypedTests where

import Control.Monad.Free (Free)
import Data.List (List(..), (:))
import Dict (KV(..))
import Prelude (Unit, discard, (#), ($))
import Result (Result(..))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert
import Untyped (Error(..), Expr(..), eval, match, mul, sub)

untypedTests :: Free TestF Unit
untypedTests = 
  suite "--== Untyped ==--" do
    suite "☯︎ match" do
      test "❌ _ <- y  ∴  Undefined variable: y" do
        match Any (Var "y") Nil # Assert.equal (Err $ UndefinedVar "y")
      test "✅ _ <- B  ∴  Γ{}" do
        match Any (Ctr "B") Nil # Assert.equal (Ok Nil)
      test "✅ 1 <- x  ∴  Γ{x: x}" do
        match (Int 1) (Var "x") ("x" `KV` Var "x" : Nil) # Assert.equal (Ok $ "x" `KV` Var "x" : Nil)
      test "✅ 1 <- 1  ∴  Γ{}" do
        match (Int 1) (Int 1) Nil # Assert.equal (Ok Nil)
      test "❌ 1 <- 2  ∴  Pattern mismatch: 1 ≠ 2" do
        match (Int 1) (Int 2) Nil # Assert.equal (Err $ PatternMismatch (Int 1) (Int 2))
      test "✅ A <- A  ∴  Γ{}" do
        match (Ctr "A") (Ctr "A") Nil # Assert.equal (Ok Nil)
      test "❌ A <- B  ∴  Pattern mismatch: A ≠ B" do
        match (Ctr "A") (Ctr "B") Nil # Assert.equal (Err $ PatternMismatch (Ctr "A") (Ctr "B"))
      test "✅ x <- B  ∴  Γ{x: B}" do
        match (Var "x") (Ctr "B") Nil # Assert.equal (Ok $ "x" `KV` Ctr "B" : Nil)
      test "✅ x <- B  Γ{x: x}  ∴  {x: B}" do
        match (Var "x") (Ctr "B") ("x" `KV` Var "x" : Nil) # Assert.equal (Ok $ "x" `KV` Ctr "B" : Nil)
      test "❌ x <- B  Γ{x: A}  ∴  Pattern mismatch: A ≠ B" do
        match (Var "x") (Ctr "B") ("x" `KV` Ctr "A" : Nil) # Assert.equal (Err $ PatternMismatch (Ctr "A") (Ctr "B"))
      test "✅ (x -> y) <- (A -> B)  ∴  Γ{y: B, x: A}" do
        match (Var "x" `To` Var "y") (Ctr "A" `To` Ctr "B") Nil # Assert.equal (Ok $ "y" `KV` Ctr "B" : "x" `KV` Ctr "A" : Nil)
      test "✅ x | y <- A  ∴  Γ{x: A}" do
        match (Var "x" `Or` Var "y") (Ctr "A") Nil # Assert.equal (Ok $ "x" `KV` Ctr "A" : Nil)
      test "✅ A | y <- B  ∴  Γ{y: B}" do
        match (Ctr "A" `Or` Var "y") (Ctr "B") Nil # Assert.equal (Ok $ "y" `KV` Ctr "B" : Nil)
      test "✅ (x, y) <- (A, B)  ∴  Γ{y: B, x: A}" do
        match (Var "x" `And` Var "y") (Ctr "A" `And` Ctr "B") Nil # Assert.equal (Ok $ "y" `KV` Ctr "B" : "x" `KV` Ctr "A" : Nil)
      test "✅ x y <- A B  ∴  Γ{y: B, x: A}" do
        match (Var "x" `App` Var "y") (Ctr "A" `App` Ctr "B") Nil # Assert.equal (Ok $ "y" `KV` Ctr "B" : "x" `KV` Ctr "A" : Nil)

    suite "☯︎ eval Any" do
      test "❌ _  ∴  Not a value: _" do
        eval Any Nil # Assert.equal (Err $ NotAValue Any)

    suite "☯︎ eval Int" do
      test "✅ 42  ∴  42" do
        eval (Int 42) Nil # Assert.equal (Ok $ Int 42)

    suite "☯︎ eval Ctr" do
      test "✅ A  ∴  A" do
        eval (Ctr "A") Nil # Assert.equal (Ok $ Ctr "A")

    suite "☯︎ eval Var" do
      test "❌ x  ∴  Undefined variable: x" do
        eval (Var "x") Nil # Assert.equal (Err $ UndefinedVar "x")
      test "✅ x  Γ{x: x}  ∴  x" do
        eval (Var "x") ("x" `KV` Var "x" : Nil) # Assert.equal (Ok $ Var "x")
      test "✅ x  Γ{y: A, x: y}  ∴  A" do
        eval (Var "x") ("y" `KV` Ctr "A" : "x" `KV` Var "y" : Nil) # Assert.equal (Ok $ Ctr "A")

    suite "☯︎ eval To" do
      test "❌ A -> _  ∴  Not a value: _" do
        eval (Ctr "A" `To` Any) Nil # Assert.equal (Err $ NotAValue Any)
      test "✅ _ -> B  ∴  _ -> B" do
        eval (Any `To` Ctr "B") Nil # Assert.equal (Ok $ Any `To` Ctr "B")
      test "✅ 1 -> B  ∴  1 -> B" do
        eval (Int 1 `To` Ctr "B") Nil # Assert.equal (Ok $ Int 1 `To` Ctr "B")
      test "✅ A -> B  ∴  A -> B" do
        eval (Ctr "A" `To` Ctr "B") Nil # Assert.equal (Ok $ Ctr "A" `To` Ctr "B")
      test "✅ x -> x  ∴  x -> x" do
        eval (Var "x" `To` Var "x") Nil # Assert.equal (Ok $ Var "x" `To` Var "x")
      test "✅ (x -> y) -> y  ∴  (x -> y) -> y" do
        eval ((Var "x" `To` Var "y") `To` Var "y") Nil # Assert.equal (Ok $ (Var "x" `To` Var "y") `To` Var "y")
      test "✅ (x | y) -> A  ∴  x -> A | y -> A" do
        eval ((Var "x" `Or` Var "y") `To` Ctr "A") Nil # Assert.equal (Ok $ Var "x" `To` Ctr "A" `Or` (Var "y" `To` Ctr "A"))
      test "✅ (x, y) -> (x, y)  ∴  (x, y) -> (x, y)" do
        eval ((Var "x" `And` Var "y") `To` (Var "x" `And` Var "y")) Nil # Assert.equal (Ok $ Var "x" `And` Var "y" `To` (Var "x" `And` Var "y"))
      test "✅ x y -> x y  ∴  x y -> x y" do
        eval ((Var "x" `App` Var "y") `To` (Var "x" `App` Var "y")) Nil # Assert.equal (Ok $ Var "x" `App` Var "y" `To` (Var "x" `App` Var "y"))

    suite "☯︎ eval Or" do
      test "❌ x | y  ∴  Undefined variable: x" do
        eval (Var "x" `Or` Var "y") Nil # Assert.equal (Err $ UndefinedVar "x")
      test "❌ A | y  ∴  Undefined variable: y" do
        eval (Ctr "A" `Or` Var "y") Nil # Assert.equal (Err $ UndefinedVar "y")
      test "✅ A | B  ∴  A | B" do
        eval (Ctr "A" `Or` Ctr "B") Nil # Assert.equal (Ok $ Ctr "A" `Or` Ctr "B")

    suite "☯︎ eval And" do
      test "❌ (x, y)  ∴  Undefined variable: x" do
        eval (Var "x" `And` Var "y") Nil # Assert.equal (Err $ UndefinedVar "x")
      test "❌ (A, y)  ∴  Undefined variable: y" do
        eval (Ctr "A" `And` Var "y") Nil # Assert.equal (Err $ UndefinedVar "y")
      test "✅ (A, B)  ∴  (A, B)" do
        eval (Ctr "A" `And` Ctr "B") Nil # Assert.equal (Ok $ Ctr "A" `And` Ctr "B")

    suite "☯︎ eval App" do
      test "❌ _ B  ∴  Not a value: _" do
        eval (Any `App` Ctr "B") Nil # Assert.equal (Err $ NotAValue Any)
      test "❌ A _  ∴  Not a value: _" do
        eval (Ctr "A" `App` Any) Nil # Assert.equal (Err $ NotAValue Any)
      test "❌ 1 B  ∴  Not a function: 1" do
        eval (Int 1 `App` Ctr "B") Nil # Assert.equal (Err $ NotAFunction (Int 1))
      test "✅ A B  ∴  A B" do
        eval (Ctr "A" `App` Ctr "B") Nil # Assert.equal (Ok $ Ctr "A" `App` Ctr "B")
      test "✅ x B  Γ{x: x}  ∴  x B" do
        eval (Var "x" `App` Ctr "B") ("x" `KV` Var "x" : Nil) # Assert.equal (Ok $ Var "x" `App` Ctr "B")
      test "✅ (_ -> B) x  Γ{x: x}  ∴  B" do
        eval (Any `To` Ctr "B" `App` Var "x") ("x" `KV` Var "x" : Nil) # Assert.equal (Ok $ Ctr "B")
      test "✅ (x -> x) y  Γ{x: x, y: y}  ∴  y" do
        eval (Var "x" `To` Var "x" `App` Var "y") ("x" `KV` Var "x" : "y" `KV` Var "y" : Nil) # Assert.equal (Ok $ Var "y")
      test "✅ (A -> B) x  Γ{x: x}  ∴  (A -> A) x" do
        eval (Ctr "A" `To` Ctr "B" `App` Var "x") ("x" `KV` Var "x" : Nil) # Assert.equal (Ok $ (Ctr "A" `To` Ctr "B") `App` Var "x")
      test "❌ (A -> B) C  ∴  Pattern mismatch: A ≠ C" do
        eval (Ctr "A" `To` Ctr "B" `App` Ctr "C") Nil # Assert.equal (Err $ PatternMismatch (Ctr "A") (Ctr "C"))
      test "✅ (A -> B) A  ∴  B" do
        eval (Ctr "A" `To` Ctr "B" `App` Ctr "A") Nil # Assert.equal (Ok $ Ctr "B")
      test "✅ (A -> C | B -> D) A  ∴  C" do
        eval ((Ctr "A" `To` Ctr "C") `Or` (Ctr "B" `To` Ctr "D") `App` Ctr "A") Nil # Assert.equal (Ok $ Ctr "C")
      test "✅ (A -> C | B -> D) B  ∴  C" do
        eval ((Ctr "A" `To` Ctr "C") `Or` (Ctr "B" `To` Ctr "D") `App` Ctr "B") Nil # Assert.equal (Ok $ Ctr "D")
      test "❌ (A -> C | B -> D) C  ∴  Pattern mismatch: B ≠ C" do
        eval ((Ctr "A" `To` Ctr "C") `Or` (Ctr "B" `To` Ctr "D") `App` Ctr "C") Nil # Assert.equal (Err $ PatternMismatch (Ctr "B") (Ctr "C"))
      test "✅ (A, B) C  ∴  (A C, B C)" do
        eval ((Ctr "A" `And` Ctr "B") `App` Ctr "C") Nil # Assert.equal (Ok $ Ctr "A" `App` Ctr "C" `And` (Ctr "B" `App` Ctr "C"))
      test "✅ A B C  ∴  A B C" do
        eval (Ctr "A" `App` Ctr "B" `App` Ctr "C") Nil # Assert.equal (Ok $ Ctr "A" `App` Ctr "B" `App` Ctr "C")

    suite "☯︎ eval Add" do
      test "✅ (+)  ∴  (+)" do
        eval Add Nil # Assert.equal (Ok Add)
      test "✅ (+) 1 2  ∴  3" do
        eval (App (App Add (Int 1)) (Int 2)) Nil # Assert.equal (Ok $ Int 3)

    suite "☯︎ eval Sub" do
      test "✅ (-)  ∴  (-)" do
        eval Sub Nil # Assert.equal (Ok Sub)
      test "✅ (-) 2 1  ∴  1" do
        eval (App (App Sub (Int 2)) (Int 1)) Nil # Assert.equal (Ok $ Int 1)

    suite "☯︎ eval Mul" do
      test "✅ (*)  ∴  (*)" do
        eval Mul Nil # Assert.equal (Ok Mul)
      test "✅ (*) 2 3  ∴  6" do
        eval (App (App Mul (Int 2)) (Int 3)) Nil # Assert.equal (Ok $ Int 6)

    suite "☯︎ eval As" do
      test "✅ 1 @ x  ∴  1" do
        eval (Int 1 `As` "x") Nil # Assert.equal (Ok $ Int 1)
      test "✅ x @ x  ∴  x" do
        eval (Var "x" `As` "x") Nil # Assert.equal (Ok $ Var "x")
      test "✅ (A x) @ x  ∴  (A x) @ x" do
        eval ((Ctr "A" `App` Var "x") `As` "x") Nil # Assert.equal (Ok $ (Ctr "A" `App` Var "x") `As` "x")

    -- suite "☯︎ Recursive definition" do
    --   test "✅ x@1  ∴  1" do
    --     reduce (Rec "x" (Num 1.0)) Nil # Assert.equal (Ok (Num 1.0))
    --   test "✅ x@x  ∴  x@x" do
    --     reduce (Rec "x" $ Var "x") Nil # Assert.equal (Ok (Rec "x" $ Var "x"))
    --   test "✅ λy. x@x  ∴  x@(λy. x)" do
    --     reduce (Lam "y" $ Rec "x" $ Var "x") Nil # Assert.equal (Ok (Rec "x" $ Lam "y" $ Var "x"))
    --   test "✅ x@x y  Γ{y: y}  ∴  x@(x y)" do
    --     reduce (App (Rec "x" $ Var "x") (Var "y")) (KV "y" (Var "y") : Nil) # Assert.equal (Ok (Rec "x" $ App (Var "x") (Var "y")))
    --   test "✅ x y@y  Γ{x: x}  ∴  y@(x y)" do
    --     reduce (App (Var "x") (Rec "y" $ Var "y")) (KV "x" (Var "x") : Nil) # Assert.equal (Ok (Rec "y" $ App (Var "x") (Var "y")))
    --   test "✅ x@x x@x  ∴  x@(x x)" do
    --     reduce (App (Rec "x" $ Var "x") (Rec "x" $ Var "x")) Nil # Assert.equal (Ok (Rec "x" $ App (Var "x") (Var "x")))
    --   test "✅ x@x y@y  ∴  x@y@(x y)" do
    --     reduce (App (Rec "x" $ Var "x") (Rec "y" $ Var "y")) Nil # Assert.equal (Ok (Rec "x" $ Rec "y" $ App (Var "x") (Var "y")))
    --   test "✅ (x@λy. x) 1  ∴  (x@λy. x) 1" do
    --     reduce (App (Rec "x" $ Lam "y" $ Var "x") (Num 1.0)) Nil # Assert.equal (Ok (App (Rec "x" $ Lam "y" $ Var "x") (Num 1.0)))

    -- suite "☯︎ Factorial" do
    --   test "✅ f  Γ{f: factorial}  ∴  f@factorial" do
    --     eval (Var "f") ("f" `KV` factorial : Nil) # Assert.equal (Ok (Var "f"))
    --   test "✅ f 0  Γ{f: factorial}  ∴  f@factorial 1" do
    --     reduce (App (Var "f") (Num 0.0)) (KV "f" factorial : Nil) # Assert.equal (Ok (App (Rec "f" factorial) (Num 0.0)))
    --   test "✅ f 0  Γ{f: factorial}  ∴  1" do
    --     eval (App (Var "f") (Num 0.0)) (KV "f" factorial : Nil) # Assert.equal (Ok (Num 1.0))
    --   test "✅ f 1  Γ{f: factorial}  ∴  1" do
    --     eval (App (Var "f") (Num 1.0)) (KV "f" factorial : Nil) # Assert.equal (Ok (Num 1.0))
    --   test "✅ f 2  Γ{f: factorial}  ∴  2" do
    --     eval (App (Var "f") (Num 2.0)) (KV "f" factorial : Nil) # Assert.equal (Ok (Num 2.0))
    --   test "✅ f 5  Γ{f: factorial}  ∴  120" do
    --     eval (App (Var "f") (Num 5.0)) (KV "f" factorial : Nil) # Assert.equal (Ok (Num 120.0))

    -- suite "☯︎ Ackermann" do
    --   test "✅ a  Γ{a: ackermann}  ∴  a@ackermann" do
    --     reduce (Var "a") (KV "a" ackermann : Nil) # Assert.equal (Ok (Rec "a" ackermann))
    --   test "✅ a 0  Γ{a: ackermann}  ∴  a@ackermann 0" do
    --     reduce (App (Var "a") (Num 0.0)) (KV "a" ackermann : Nil) # Assert.equal (Ok (App (Rec "a" ackermann) (Num 0.0)))
    --   test "✅ a 0  Γ{a: ackermann}  ∴  λn. n + 1" do
    --     eval (App (Var "a") (Num 0.0)) (KV "a" ackermann : Nil) # Assert.equal (Ok (Lam "n" $ add (Var "n") (Num 1.0)))
    --   test "✅ a 0 0  Γ{a: ackermann}  ∴  1" do
    --     eval (app2 (Var "a") (Num 0.0) (Num 0.0)) (KV "a" ackermann : Nil) # Assert.equal (Ok (Num 1.0))
    --   test "✅ a 1 1  Γ{a: ackermann}  ∴  3" do
    --     eval (app2 (Var "a") (Num 1.0) (Num 1.0)) (KV "a" ackermann : Nil) # Assert.equal (Ok (Num 3.0))
    --   test "✅ a 2 2  Γ{a: ackermann}  ∴  7" do
    --     eval (app2 (Var "a") (Num 2.0) (Num 2.0)) (KV "a" ackermann : Nil) # Assert.equal (Ok (Num 7.0))
    --   test "✅ a 3 2  Γ{a: ackermann}  ∴  29" do
    --     eval (app2 (Var "a") (Num 3.0) (Num 2.0)) (KV "a" ackermann : Nil) # Assert.equal (Ok (Num 29.0))

    where
      -- f 0 = 1
      -- f n = n * f (n - 1)
      factorial =
        Int 0 `To` Int 1
        `Or` (Var "n" `To` (Var "n" `mul` (Var "f" `App` (Var "n" `sub` Int 1))))

    --   -- a 0 n = n + 1
    --   -- a m 0 = a (m-1) 1
    --   -- a m n = a (m-1) (a m (n-1))
    --   ackermann = Lam "m" $ Lam "n"
    --     (app2 (eq m k0) (add n k1)
    --     (app2 (eq n k0) (app2 a (sub m k1) k1)
    --     (app2 a (sub m k1) (app2 a m (sub n k1)))))
