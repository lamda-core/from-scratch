module Test.UntypedTests where

import Control.Monad.Free (Free)
import Data.List (List(..))
import Data.Tuple (Tuple(..))
import Prelude (Unit, discard, negate, (#), ($))
import Result (Result(..))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert
import Untyped (Error(..), Expr(..), add, app2, eq, eval, mul, reduce, sub)

untypedTests :: Free TestF Unit
untypedTests = 
  suite "--== Untyped ==--" do
    suite "☯︎ Number" do
      test "✅ 1  ∴  1" do
        reduce (Num 1.0) Nil # Assert.equal (Ok (Num 1.0))

    suite "☯︎ Variable" do
      test "❌ x  ∴  Undefined variable: x" do
        reduce (Var "x") Nil # Assert.equal (Err (UndefinedVar "x"))
      test "✅ x  Γ{x: x}  ∴  x" do
        reduce (Var "x") (Cons (Tuple "x" (Var "x")) Nil) # Assert.equal (Ok (Var "x"))
      test "✅ x  Γ{y: 1, x: y}  ∴  1" do
        reduce (Var "x") (Cons (Tuple "y" (Num 1.0)) $ Cons (Tuple "x" (Var "y")) Nil) # Assert.equal (Ok (Num 1.0))

    suite "☯︎ Lamda abstraction" do
      test "❌ λx. y  ∴  Undefined variable: y" do
        reduce (Lam "x" (Var "y")) Nil # Assert.equal (Err (UndefinedVar "y"))
      test "✅ λx. x  ∴  λx. x" do
        reduce (Lam "x" (Var "x")) Nil # Assert.equal (Ok (Lam "x" (Var "x")))

    suite "☯︎ Application" do
      test "❌ x y  ∴  Undefined variable: x" do
        reduce (App (Var "x") (Var "y")) Nil # Assert.equal (Err (UndefinedVar "x"))
      test "❌ x y  Γ{x: x}  ∴  Undefined variable: y" do
        reduce (App (Var "x") (Var "y")) (Cons (Tuple "x" (Var "x")) Nil) # Assert.equal (Err (UndefinedVar "y"))
      test "❌ 1 2  ∴  Not a function: 1" do
        reduce (App (Num 1.0) (Num 2.0)) Nil # Assert.equal (Err (NotAFunction (Num 1.0))) 
      test "✅ f 1  Γ{f: f}  ∴  f 1" do
        reduce (App (Var "f") (Num 1.0)) (Cons (Tuple "f" (Var "f")) Nil) # Assert.equal (Ok (App (Var "f") (Num 1.0)))
      test "✅ (λx. x) 1  ∴  1" do
        reduce (App (Lam "x" (Var "x")) (Num 1.0)) Nil # Assert.equal (Ok (Num 1.0))
      test "✅ ((λx. x) (λy. y)) 1  ∴  1" do
        reduce (App (App (Lam "x" (Var "x")) (Lam "y" (Var "y"))) (Num 1.0)) Nil # Assert.equal (Ok (Num 1.0))

    suite "☯︎ Addition" do
      test "✅ (+)  ∴  (+)" do
        reduce Add Nil # Assert.equal (Ok Add)
      test "✅ (+) 1 2  ∴  3" do
        reduce (App (App Add (Num 1.0)) (Num 2.0)) Nil # Assert.equal (Ok (Num 3.0))

    suite "☯︎ Subtraction" do
      test "✅ (-)  ∴  (-)" do
        reduce Sub Nil # Assert.equal (Ok Sub)
      test "✅ (-) 1 2  ∴  -1" do
        reduce (App (App Sub (Num 1.0)) (Num 2.0)) Nil # Assert.equal (Ok (Num (-1.0)))

    suite "☯︎ Multiplication" do
      test "✅ (*)  ∴  (*)" do
        reduce Mul Nil # Assert.equal (Ok Mul)
      test "✅ (*) 1 2  ∴  2" do
        reduce (App (App Mul (Num 1.0)) (Num 2.0)) Nil # Assert.equal (Ok (Num 2.0))

    suite "☯︎ Equals" do
      test "✅ (==)  ∴  (==)" do
        reduce Eq Nil # Assert.equal (Ok Eq)
      test "✅ (==) 1 1  ∴  λTrue False. True" do
        reduce (App (App Eq (Num 1.0)) (Num 1.0)) Nil # Assert.equal (Ok (Lam "True" $ Lam "False" $ Var "True"))
      test "✅ (==) 1 2  ∴  λTrue False. False" do
        reduce (App (App Eq (Num 1.0)) (Num 2.0)) Nil # Assert.equal (Ok (Lam "True" $ Lam "False" $ Var "False"))

    suite "☯︎ Recursive definition" do
      test "✅ x@1  ∴  1" do
        reduce (Rec "x" (Num 1.0)) Nil # Assert.equal (Ok (Num 1.0))
      test "✅ x@x  ∴  x@x" do
        reduce (Rec "x" $ Var "x") Nil # Assert.equal (Ok (Rec "x" $ Var "x"))
      test "✅ λy. x@x  ∴  x@(λy. x)" do
        reduce (Lam "y" $ Rec "x" $ Var "x") Nil # Assert.equal (Ok (Rec "x" $ Lam "y" $ Var "x"))
      test "✅ x@x y  Γ{y: y}  ∴  x@(x y)" do
        reduce (App (Rec "x" $ Var "x") (Var "y")) (Cons (Tuple "y" (Var "y")) Nil) # Assert.equal (Ok (Rec "x" $ App (Var "x") (Var "y")))
      test "✅ x y@y  Γ{x: x}  ∴  y@(x y)" do
        reduce (App (Var "x") (Rec "y" $ Var "y")) (Cons (Tuple "x" (Var "x")) Nil) # Assert.equal (Ok (Rec "y" $ App (Var "x") (Var "y")))
      test "✅ x@x x@x  ∴  x@(x x)" do
        reduce (App (Rec "x" $ Var "x") (Rec "x" $ Var "x")) Nil # Assert.equal (Ok (Rec "x" $ App (Var "x") (Var "x")))
      test "✅ x@x y@y  ∴  x@y@(x y)" do
        reduce (App (Rec "x" $ Var "x") (Rec "y" $ Var "y")) Nil # Assert.equal (Ok (Rec "x" $ Rec "y" $ App (Var "x") (Var "y")))
      test "✅ (x@λy. x) 1  ∴  (x@λy. x) 1" do
        reduce (App (Rec "x" $ Lam "y" $ Var "x") (Num 1.0)) Nil # Assert.equal (Ok (App (Rec "x" $ Lam "y" $ Var "x") (Num 1.0)))

    suite "☯︎ Factorial" do
      test "✅ f  Γ{f: factorial}  ∴  f@factorial" do
        reduce (Var "f") (Cons (Tuple "f" factorial) Nil) # Assert.equal (Ok (Rec "f" factorial))
      test "✅ f 0  Γ{f: factorial}  ∴  f@factorial 1" do
        reduce (App (Var "f") (Num 0.0)) (Cons (Tuple "f" factorial) Nil) # Assert.equal (Ok (App (Rec "f" factorial) (Num 0.0)))
      test "✅ f 0  Γ{f: factorial}  ∴  1" do
        eval (App (Var "f") (Num 0.0)) (Cons (Tuple "f" factorial) Nil) # Assert.equal (Ok (Num 1.0))
      test "✅ f 1  Γ{f: factorial}  ∴  1" do
        eval (App (Var "f") (Num 1.0)) (Cons (Tuple "f" factorial) Nil) # Assert.equal (Ok (Num 1.0))
      test "✅ f 2  Γ{f: factorial}  ∴  2" do
        eval (App (Var "f") (Num 2.0)) (Cons (Tuple "f" factorial) Nil) # Assert.equal (Ok (Num 2.0))
      test "✅ f 5  Γ{f: factorial}  ∴  120" do
        eval (App (Var "f") (Num 5.0)) (Cons (Tuple "f" factorial) Nil) # Assert.equal (Ok (Num 120.0))

    suite "☯︎ Ackermann" do
      test "✅ a  Γ{a: ackermann}  ∴  a@ackermann" do
        reduce (Var "a") (Cons (Tuple "a" ackermann) Nil) # Assert.equal (Ok (Rec "a" ackermann))
      test "✅ a 0  Γ{a: ackermann}  ∴  a@ackermann 0" do
        reduce (App (Var "a") (Num 0.0)) (Cons (Tuple "a" ackermann) Nil) # Assert.equal (Ok (App (Rec "a" ackermann) (Num 0.0)))
      test "✅ a 0  Γ{a: ackermann}  ∴  λn. n + 1" do
        eval (App (Var "a") (Num 0.0)) (Cons (Tuple "a" ackermann) Nil) # Assert.equal (Ok (Lam "n" $ add (Var "n") (Num 1.0)))
      test "✅ a 0 0  Γ{a: ackermann}  ∴  1" do
        eval (app2 (Var "a") (Num 0.0) (Num 0.0)) (Cons (Tuple "a" ackermann) Nil) # Assert.equal (Ok (Num 1.0))
      test "✅ a 1 1  Γ{a: ackermann}  ∴  3" do
        eval (app2 (Var "a") (Num 1.0) (Num 1.0)) (Cons (Tuple "a" ackermann) Nil) # Assert.equal (Ok (Num 3.0))
      test "✅ a 2 2  Γ{a: ackermann}  ∴  7" do
        eval (app2 (Var "a") (Num 2.0) (Num 2.0)) (Cons (Tuple "a" ackermann) Nil) # Assert.equal (Ok (Num 7.0))
      test "✅ a 3 2  Γ{a: ackermann}  ∴  29" do
        eval (app2 (Var "a") (Num 3.0) (Num 2.0)) (Cons (Tuple "a" ackermann) Nil) # Assert.equal (Ok (Num 29.0))

    where
      a = Var "a"
      f = Var "f"
      m = Var "m"
      n = Var "n"
      k0 = Num 0.0
      k1 = Num 1.0

      -- f 0 = 1
      -- f n = n * f (n - 1)
      factorial = Lam "n" $
        app2 (eq n k0) k1 (mul n (App f (sub n k1)))

      -- a 0 n = n + 1
      -- a m 0 = a (m-1) 1
      -- a m n = a (m-1) (a m (n-1))
      ackermann = Lam "m" $ Lam "n"
        (app2 (eq m k0) (add n k1)
        (app2 (eq n k0) (app2 a (sub m k1) k1)
        (app2 a (sub m k1) (app2 a m (sub n k1)))))
