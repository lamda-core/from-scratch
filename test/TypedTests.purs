module Test.TypedTests where

import Control.Monad.Free (Free)
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))
import Prelude (Unit, discard, (#), ($))
import Result (Result(..))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert
import Typed (Error(..), Expr(..), Typ(..), reduce, reduceT, unify)

typedTests :: Free TestF Unit
typedTests = 
  suite "--== Typed ==--" do
    suite "☯︎ Number type" do
      test "✅ Num  ∴  Num" do
        reduceT NumT Nil # Assert.equal (Ok NumT)

    suite "☯︎ Variable type" do
      test "❌ x  ∴  Undefined variable: x" do
        reduceT (VarT "x") Nil # Assert.equal (Err $ UndefinedVar "x")
      test "✅ x  Γ{x: x}  ∴  x" do
        reduceT (VarT "x") (Tuple "x" (VarT "x") : Nil) # Assert.equal (Ok $ VarT "x")
      test "✅ x  Γ{y: 1, x: y}  ∴  1" do
        reduceT (VarT "x") (Tuple "y" NumT : Tuple "x" (VarT "y") : Nil) # Assert.equal (Ok NumT)

    suite "☯︎ Function type" do
      test "❌ x -> y  ∴  Undefined variable: x" do
        reduceT (FunT (VarT "x") (VarT "y")) Nil # Assert.equal (Err $ UndefinedVar "x")
      test "❌ Num -> x  ∴  Undefined variable: x" do
        reduceT (FunT NumT (VarT "x")) Nil # Assert.equal (Err $ UndefinedVar "x")
      test "✅ Num -> Num  ∴  Num -> Num" do
        reduceT (FunT NumT NumT) Nil # Assert.equal (Ok $ FunT NumT NumT)

    suite "☯︎ For all -- generic type" do
      test "✅ ∀x. Num  ∴  Num" do
        reduceT (For "x" NumT) Nil # Assert.equal (Ok NumT)
      test "✅ ∀x. x  ∴  ∀x. x" do
        reduceT (For "x" $ VarT "x") Nil # Assert.equal (Ok $ For "x" $ VarT "x")
      test "✅ (∀x. x) -> Num  ∴  ∀x. x -> Num" do
        reduceT (FunT (For "x" $ VarT "x") NumT) Nil # Assert.equal (Ok $ For "x" $ FunT (VarT "x") NumT)
      test "✅ Num -> (∀x. x)  ∴  ∀x. Num -> x" do
        reduceT (FunT NumT (For "x" $ VarT "x")) Nil # Assert.equal (Ok $ For "x" $ FunT NumT (VarT "x"))
      test "✅ (∀x. x) -> (∀x. x)  ∴  ∀x. x -> x" do
        reduceT (FunT (For "x" $ VarT "x") (For "x" $ VarT "x")) Nil # Assert.equal (Ok $ For "x" $ FunT (VarT "x") (VarT "x"))
      test "✅ (∀x. x) -> (∀y. y)  ∴  ∀x y. x -> y" do
        reduceT (FunT (For "x" $ VarT "x") (For "y" $ VarT "y")) Nil # Assert.equal (Ok $ For "x" $ For "y" $ FunT (VarT "x") (VarT "y"))

    suite "☯︎ Unification" do
      test "❌ x == y  ∴  Undefined variable: x" do
        unify (VarT "x") (VarT "y") Nil # Assert.equal (Err $ UndefinedVar "x")
      test "❌ x == y  Γ{x: x}  ∴  Undefined variable: y" do
        unify (VarT "x") (VarT "y") (Tuple "x" (VarT "x") : Nil) # Assert.equal (Err $ UndefinedVar "y")
      test "❌ x == Num  Γ{x: x}  ∴  Type mismatch: x ≠ Num" do
        unify (VarT "x") NumT (Tuple "x" (VarT "x") : Nil) # Assert.equal (Err $ TypeMismatch (VarT "x") NumT)
      test "✅ Num == Num  ∴  Num" do
        unify NumT NumT Nil # Assert.equal (Ok $ Tuple NumT Nil)
      test "❌ x -> x == Num -> x  Γ{x: x}  ∴  Type mismatch: x ≠ Num" do
        unify (FunT (VarT "x") (VarT "x")) (FunT NumT (VarT "x")) (Tuple "x" (VarT "x") : Nil) # Assert.equal (Err $ TypeMismatch (VarT "x") NumT)
      test "❌ Num -> x == Num -> Num  Γ{x: x}  ∴  Type mismatch: x ≠ Num" do
        unify (FunT NumT (VarT "x")) (FunT NumT NumT) (Tuple "x" (VarT "x") : Nil) # Assert.equal (Err $ TypeMismatch (VarT "x") NumT)
      test "✅ Num -> Num == Num -> Num  ∴  Num -> Num" do
        unify (FunT NumT NumT) (FunT NumT NumT) Nil # Assert.equal (Ok $ Tuple (FunT NumT NumT) Nil)
      test "✅ ∀x. x == Num  ∴  Num  Γ{x: Num}" do
        unify (For "x" $ VarT "x") NumT Nil # Assert.equal (Ok $ Tuple NumT $ Tuple "x" NumT : Nil)
      test "✅ Num == ∀x. x  ∴  Num  Γ{x: Num}" do
        unify NumT (For "x" $ VarT "x") Nil # Assert.equal (Ok $ Tuple NumT $ Tuple "x" NumT : Nil)
      test "✅ ∀x. x == ∀y. y  ∴  ∀y. y  Γ{x: y, y: ∀y. y}" do
        unify (For "x" $ VarT "x") (For "y" $ VarT "y") Nil # Assert.equal (Ok $ Tuple (For "y" $ VarT "y") $ Tuple "x" (VarT "y") : Tuple "y" (For "y" $ VarT "y") : Nil)
      test "✅ ∀x. x -> x == Num -> Num  ∴  Num -> Num  Γ{x: Num}" do
        unify (For "x" $ FunT (VarT "x") (VarT "x")) (FunT NumT NumT) Nil # Assert.equal (Ok $ Tuple (FunT NumT NumT) $ Tuple "x" NumT : Nil)
      test "✅ Num -> Num == ∀x. x -> x  ∴  Num -> Num  Γ{x: Num}" do
        unify (FunT NumT NumT) (For "x" $ FunT (VarT "x") (VarT "x")) Nil # Assert.equal (Ok $ Tuple (FunT NumT NumT) $ Tuple "x" NumT : Nil)
      test "✅ ∀x. x -> x == ∀y. y -> y  ∴  ∀y. y -> y  Γ{x: y, y: ∀y. y}" do
        unify (For "x" $ FunT (VarT "x") (VarT "x")) (For "y" $ FunT (VarT "y") (VarT "y")) Nil # Assert.equal (Ok $ Tuple ((For "y" $ FunT (VarT "y") (VarT "y"))) $ Tuple "x" (VarT "y") : Tuple "y" (For "y" $ VarT "y") : Nil)

    suite "☯︎ Number" do
      test "✅ 1  ∴  1 : Num" do
        reduce (Num 1.0) Nil Nil # Assert.equal (Ok $ Tuple (Num 1.0) NumT)

    suite "☯︎ Variable" do
      test "❌ x  ∴  Undefined variable: x" do
        reduce (Var "x") Nil Nil # Assert.equal (Err $ UndefinedVar "x")
      test "❌ x  Γ{x: x}  ∴  Missing type: x" do
        reduce (Var "x") (Tuple "x" (Var "x") : Nil) Nil # Assert.equal (Err $ MissingType "x")
      test "✅ x  Γ{x: x : Num}  ∴  x : ∀x. x" do
        reduce (Var "x") (Tuple "x" (Ann (Var "x") NumT) : Nil) Nil # Assert.equal (Ok $ Tuple (Var "x") NumT)
      test "✅ x  Γ{y: 1, x: y}  ∴  1 : Num" do
        reduce (Var "x") (Tuple "y" (Num 1.0) : Tuple "x" (Var "y") : Nil) Nil # Assert.equal (Ok $ Tuple (Num 1.0) NumT)

    suite "☯︎ Type annotation" do
      test "❌ x : y  ∴  Undefined variable: x" do
        reduce (Ann (Var "x") (VarT "y")) Nil Nil # Assert.equal (Err $ UndefinedVar "x")
      test "❌ 1 : x  ∴  Undefined variable: y" do
        reduce (Ann (Num 1.0) (VarT "x")) Nil Nil # Assert.equal (Err $ UndefinedVar "x")
      test "❌ 1 : Num -> Num  ∴  Type mismatch: Num ≠ Num -> Num" do
        reduce (Ann (Num 1.0) (FunT NumT NumT)) Nil Nil # Assert.equal (Err $ TypeMismatch NumT (FunT NumT NumT))
      test "✅ 1 : Num  ∴  1 : Num" do
        reduce (Ann (Num 1.0) NumT) Nil Nil # Assert.equal (Ok $ Tuple (Num 1.0) NumT)
      test "✅ λx. x : Num -> Num  ∴  λx. x : Num -> Num" do
        reduce (Ann (Lam "x" $ Var "x") (FunT NumT NumT)) Nil Nil # Assert.equal (Ok $ Tuple (Lam "x" $ Var "x") (FunT NumT NumT))
      test "✅ λx. x : ∀y. y -> Num  ∴  λx. x : Num -> Num" do
        reduce (Ann (Lam "x" $ Var "x") (For "y" $ FunT (VarT "y") NumT)) Nil Nil # Assert.equal (Ok $ Tuple (Lam "x" $ Var "x") (FunT NumT NumT))

    suite "☯︎ Lamda abstraction" do
      test "❌ λx. y  ∴  Undefined variable: y" do
        reduce (Lam "x" $ Var "y") Nil Nil # Assert.equal (Err $ UndefinedVar "y")
      test "✅ λx. 1  ∴  λx. x : ∀x. x -> Num" do
        reduce (Lam "x" $ Num 1.0) Nil Nil # Assert.equal (Ok $ Tuple (Lam "x" $ Num 1.0) (For "x" $ FunT (VarT "x") NumT))
      test "✅ λx. x  ∴  λx. x : ∀x. x -> x" do
        reduce (Lam "x" $ Var "x") Nil Nil # Assert.equal (Ok $ Tuple (Lam "x" $ Var "x") (For "x" $ FunT (VarT "x") (VarT "x")))

    suite "☯︎ Application" do
      test "❌ x y  ∴  Undefined variable: x" do
        reduce (App (Var "x") (Var "y")) Nil Nil # Assert.equal (Err $ UndefinedVar "x")
      test "❌ 1 2  ∴  Not a function: 1 : Num" do
        reduce (App (Num 1.0) (Num 2.0)) Nil Nil # Assert.equal (Err $ NotAFunction (Num 1.0) NumT)
      test "❌ x y  Γ{x: x : Num -> Num}  ∴  Undefined variable: y" do
        reduce (App (Var "x") (Var "y")) (Tuple "x" (Ann (Var "x") (FunT NumT NumT)) : Nil) Nil # Assert.equal (Err $ UndefinedVar "y")
      test "❌ x x  Γ{x: x : Num -> Num}  ∴  Type mismatch: Num ≠ Num -> Num" do
        reduce (App (Var "x") (Var "x")) (Tuple "x" (Ann (Var "x") (FunT NumT NumT)) : Nil) Nil # Assert.equal (Err $ TypeMismatch NumT (FunT NumT NumT))
      test "✅ x 1  Γ{x: x : Num -> Num}  ∴  x 1 : Num" do
        reduce (App (Var "x") (Num 1.0)) (Tuple "x" (Ann (Var "x") (FunT NumT NumT)) : Nil) Nil # Assert.equal (Ok $ Tuple (App (Var "x") (Num 1.0)) NumT)
      test "✅ x 1  Γ{x: x : ∀x. x -> x}  ∴  x 1 : Num" do
        reduce (App (Var "x") (Num 1.0)) (Tuple "x" (Ann (Var "x") (For "x" $ FunT (VarT "x") (VarT "x"))) : Nil) Nil # Assert.equal (Ok $ Tuple (App (Var "x") (Num 1.0)) NumT)
      test "❌ (1 : Num -> Num) 1  ∴  Type mismatch: Num ≠ Num -> Num" do
        reduce (App (Ann (Num 1.0) (FunT NumT NumT)) (Num 1.0)) Nil Nil # Assert.equal (Err $ TypeMismatch (FunT NumT NumT) NumT)
      test "✅ (λx. x) 1  ∴  1 : Num" do
        reduce (App (Lam "x" $ Var "x") (Num 1.0)) Nil Nil # Assert.equal (Ok $ Tuple (Num 1.0) NumT)
      test "✅ ((λx. x) (λy. y)) 1  ∴  1" do
        reduce (App (App (Lam "x" (Var "x")) (Lam "y" (Var "y"))) (Num 1.0)) Nil Nil # Assert.equal (Ok $ Tuple (Num 1.0) NumT)

    -- suite "☯︎ Addition" do
    --   test "✅ (+)  ∴  (+)" do
    --     reduce Add Nil # Assert.equal (Ok Add)
    --   test "✅ (+) 1 2  ∴  3" do
    --     reduce (App (App Add (Num 1.0)) (Num 2.0)) Nil # Assert.equal (Ok (Num 3.0))

    -- suite "☯︎ Subtraction" do
    --   test "✅ (-)  ∴  (-)" do
    --     reduce Sub Nil # Assert.equal (Ok Sub)
    --   test "✅ (-) 1 2  ∴  -1" do
    --     reduce (App (App Sub (Num 1.0)) (Num 2.0)) Nil # Assert.equal (Ok (Num (-1.0)))

    -- suite "☯︎ Multiplication" do
    --   test "✅ (*)  ∴  (*)" do
    --     reduce Mul Nil # Assert.equal (Ok Mul)
    --   test "✅ (*) 1 2  ∴  2" do
    --     reduce (App (App Mul (Num 1.0)) (Num 2.0)) Nil # Assert.equal (Ok (Num 2.0))

    -- suite "☯︎ Equals" do
    --   test "✅ (==)  ∴  (==)" do
    --     reduce Eq Nil # Assert.equal (Ok Eq)
    --   test "✅ (==) 1 1  ∴  λTrue False. True" do
    --     reduce (App (App Eq (Num 1.0)) (Num 1.0)) Nil # Assert.equal (Ok (Lam "True" $ Lam "False" $ Var "True"))
    --   test "✅ (==) 1 2  ∴  λTrue False. False" do
    --     reduce (App (App Eq (Num 1.0)) (Num 2.0)) Nil # Assert.equal (Ok (Lam "True" $ Lam "False" $ Var "False"))

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
