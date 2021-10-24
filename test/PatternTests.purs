module Test.PatternTests where

import Control.Monad.Free (Free)
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))
import Pattern (Error(..), Expr(..), eval)
import Prelude (Unit, discard, (#), ($))
import Result (Result(..))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

patternTests :: Free TestF Unit
patternTests = 
  suite "--== Pattern calculus ==--" do
    suite "☯︎ Anything" do
      test "✅ \x  ∴  \x" do
        eval (Any "x") Nil # Assert.equal (Ok $ Any "x")

    suite "☯︎ Constructor" do
      test "✅ A  ∴  A" do
        eval (Ctr "A") Nil # Assert.equal (Ok $ Ctr "A")

    suite "☯︎ Variable" do
      test "❌ x  ∴  Undefined variable: x" do
        eval (Var "x") Nil # Assert.equal (Err $ UndefinedVar "x")
      test "✅ x  Γ{x: x}  ∴  x" do
        eval (Var "x") (Tuple "x" (Var "x") : Nil) # Assert.equal (Ok $ Var "x")
      test "✅ x  Γ{y: A, x: y}  ∴  A" do
        eval (Var "x") (Tuple "y" (Ctr "A") : Tuple "x" (Var "y") : Nil) # Assert.equal (Ok $ Ctr "A")

    suite "☯︎ Lamda case" do
      test "❌ _ -> x  ∴  Undefined variable: x" do
        eval (Lam (Any "") (Var "x")) Nil # Assert.equal (Err $ UndefinedVar "x")
      test "✅ _ -> A  ∴  _ -> A" do
        eval (Lam (Any "") (Ctr "A")) Nil # Assert.equal (Ok $ Lam (Any "") (Ctr "A"))
      test "✅ \x -> x  ∴  \x -> x" do
        eval (Lam (Any "x") (Var "x")) Nil # Assert.equal (Ok $ Lam (Any "x") (Var "x"))

    suite "☯︎ Application" do
      test "❌ _ B  ∴  Not a function: _" do
        eval (App (Any "") (Ctr "B")) Nil # Assert.equal (Err $ NotAFunction (Any ""))
      test "✅ A B  ∴  A B" do
        eval (App (Ctr "A") (Ctr "B")) Nil # Assert.equal (Ok $ App (Ctr "A") (Ctr "B"))
      test "❌ x B  ∴  Undefined variable: x" do
        eval (App (Var "x") (Ctr "B")) Nil # Assert.equal (Err $ UndefinedVar "x")
      test "❌ x y  Γ{x: x}  ∴  Undefined variable: y" do
        eval (App (Var "x") (Var "y")) (Tuple "x" (Ctr "A") : Nil) # Assert.equal (Err $ UndefinedVar "y")
      test "❌ x B  Γ{x: x}  ∴  x B" do
        eval (App (Var "x") (Ctr "B")) (Tuple "x" (Var "x") : Nil) # Assert.equal (Ok $ App (Var "x") (Ctr "B"))
      test "✅ (x -> x) A  ∴  A" do
        eval (App (Lam (Any "x") (Var "x")) (Ctr "A")) Nil # Assert.equal (Ok $ Ctr "A")

    -- suite "☯︎ Application" do
    --   test "❌ x _  Γ{x: *}  ∴  Not a function: x" do
    --     eval (App (Var "x") Any) (Tuple "x" Ctr : Nil) # Assert.equal (Err $ NotAFunction (Var "x"))
    --   test "❌ (_ -> x) y  Γ{x: *}  ∴  Undefined variable: y" do
    --     eval (App (Lam Any (Var "x")) (Var "y")) (Tuple "x" Ctr : Nil) # Assert.equal (Err $ UndefinedVar "y")
    --   test "❌ (_ -> x) y  Γ{y: *}  ∴  Undefined variable: x" do
    --     eval (App (Lam Any (Var "x")) (Var "y")) (Tuple "y" Ctr : Nil) # Assert.equal (Err $ UndefinedVar "x")
    --   test "✅ (_ -> x) y  Γ{x: *, y: *}  ∴  x" do
    --     eval (App (Lam Any (Var "x")) (Var "y")) (Tuple "x" Ctr : Tuple "y" Ctr : Nil) # Assert.equal (Ok $ Var "x")
    --   test "✅ (x -> x) y  Γ{y: *}  ∴  y" do
    --     eval (App (Lam (Var "x") (Var "x")) (Var "y")) (Tuple "y" Ctr : Nil) # Assert.equal (Ok $ Var "y")

    suite "☯︎ Or -- Union / Sum type / Disjunction" do
      test "❌ x | y  ∴  Undefined variable: x" do
        eval (Or (Var "x") (Var "y")) Nil # Assert.equal (Err $ UndefinedVar "x")
      test "❌ A | y  ∴  Undefined variable: y" do
        eval (Or (Ctr "A") (Var "y")) Nil # Assert.equal (Err $ UndefinedVar "y")
      test "✅ A | B  ∴  A | B" do
        eval (Or (Ctr "A") (Ctr "B")) Nil # Assert.equal (Ok $ Or (Ctr "A") (Ctr "B"))

    suite "☯︎ And -- Tuple / Product type / Conjunction" do
      test "❌ (x, y)  ∴  Undefined variable: x" do
        eval (And (Var "x") (Var "y")) Nil # Assert.equal (Err $ UndefinedVar "x")
      test "❌ (A, y)  ∴  Undefined variable: y" do
        eval (And (Ctr "A") (Var "y")) Nil # Assert.equal (Err $ UndefinedVar "y")
      test "✅ (A, B)  ∴  (A, B)" do
        eval (And (Ctr "A") (Ctr "B")) Nil # Assert.equal (Ok $ And (Ctr "A") (Ctr "B"))

    -- suite "☯︎ Number type" do
    --   test "✅ Num  ∴  Num" do
    --     reduceT NumT Nil # Assert.equal (Ok NumT)

    -- suite "☯︎ Variable type" do
    --   test "❌ A  ∴  Undefined variable: A" do
    --     reduceT (VarT "A") Nil # Assert.equal (Err $ UndefinedVar "A")
    --   test "✅ A  ∏{A: A}  ∴  A" do
    --     reduceT (VarT "A") (Tuple "A" (VarT "A") : Nil) # Assert.equal (Ok $ VarT "A")
    --   test "✅ A  ∏{B: Int, A: B}  ∴  1" do
    --     reduceT (VarT "A") (Tuple "B" IntT : Tuple "A" (VarT "B") : Nil) # Assert.equal (Ok IntT)

    -- suite "☯︎ For all" do
    --   test "❌ ∀a. b  ∴  Undefined variable: b" do
    --     reduceT (For "a" $ VarT "b") Nil # Assert.equal (Err $ UndefinedVar "b")
    --   test "✅ ∀a. a  ∴  ∀a. a" do
    --     reduceT (For "a" $ VarT "a") Nil # Assert.equal (Ok $ For "a" $ VarT "a")

    -- suite "☯︎ Function type" do
    --   test "❌ A -> B  ∴  Undefined variable: A" do
    --     reduceT (FunT (VarT "A") (VarT "B")) Nil # Assert.equal (Err $ UndefinedVar "A")
    --   test "❌ A -> B  ∏{A: A}  ∴  Undefined variable: B" do
    --     reduceT (FunT (VarT "A") (VarT "B")) (Tuple "A" (VarT "A") : Nil) # Assert.equal (Err $ UndefinedVar "B")
    --   test "✅ A -> B  ∏{A: A, B: B}  ∴  A -> B" do
    --     reduceT (FunT (VarT "A") (VarT "B")) (Tuple "A" (VarT "A") : Tuple "B" (VarT "B") : Nil) # Assert.equal (Ok $ FunT (VarT "A") (VarT "B"))

    -- suite "☯︎ Sum type" do
    --   test "❌ A | B  ∴  Undefined variable: A" do
    --     reduceT (OrT (VarT "A") (VarT "B")) Nil # Assert.equal (Err $ UndefinedVar "A")
    --   test "❌ A | B  ∏{A: A}  ∴  Undefined variable: B" do
    --     reduceT (OrT (VarT "A") (VarT "B")) (Tuple "A" (VarT "A") : Nil) # Assert.equal (Err $ UndefinedVar "B")
    --   test "✅ A | B  ∏{A: A, B: B}  ∴  A | B" do
    --     reduceT (OrT (VarT "A") (VarT "B")) (Tuple "A" (VarT "A") : Tuple "B" (VarT "B") : Nil) # Assert.equal (Ok $ OrT (VarT "A") (VarT "B"))

    -- suite "☯︎ Product type" do
    --   test "❌ (A, B)  ∴  Undefined variable: A" do
    --     reduceT (AndT (VarT "A") (VarT "B")) Nil # Assert.equal (Err $ UndefinedVar "A")
    --   test "❌ (A, B)  ∏{A: A}  ∴  Undefined variable: B" do
    --     reduceT (AndT (VarT "A") (VarT "B")) (Tuple "A" (VarT "A") : Nil) # Assert.equal (Err $ UndefinedVar "B")
    --   test "✅ (A, B)  ∏{A: A, B: B}  ∴  (A, B)" do
    --     reduceT (AndT (VarT "A") (VarT "B")) (Tuple "A" (VarT "A") : Tuple "B" (VarT "B") : Nil) # Assert.equal (Ok $ AndT (VarT "A") (VarT "B"))

    -- suite "☯︎ Unification" do
    --   test "❌ A == B  ∴  Undefined variable: A" do
    --     unify (VarT "A") (VarT "B") Nil # Assert.equal (Err $ UndefinedVar "A")
    --   test "❌ Int == A  ∴  Undefined variable: A" do
    --     unify IntT (VarT "A") Nil # Assert.equal (Err $ UndefinedVar "A")
    --   test "❌ Int == Num  ∴  Type mismatch: Int ≠ Num" do
    --     unify IntT NumT Nil # Assert.equal (Err $ TypeMismatch IntT NumT)
    --   test "✅ Int == Int  ∴  Int" do
    --     unify IntT IntT Nil # Assert.equal (Ok $ Tuple IntT Nil)
    --   test "✅ A == Int  ∏{A: A}  ∴  Int  ∏{A: Int}" do
    --     unify (VarT "A") IntT (Tuple "A" (VarT "A") : Nil) # Assert.equal (Ok $ Tuple IntT (Tuple "A" IntT : Nil))
    --   test "✅ Int == A  ∏{A: A}  ∴  Int  ∏{A: Int}" do
    --     unify IntT (VarT "A") (Tuple "A" (VarT "A") : Nil) # Assert.equal (Ok $ Tuple IntT (Tuple "A" IntT : Nil))
    --   test "✅ A -> Int == Int -> Int  ∏{A: A}  ∴  Int -> Int  ∏{A: Int}" do
    --     unify (FunT (VarT "A") IntT) (FunT IntT IntT) (Tuple "A" IntT : Nil) # Assert.equal (Ok $ Tuple (FunT IntT IntT) (Tuple "A" IntT : Nil))
    --   test "✅ Int -> A == Int -> Int  ∏{A: A}  ∴  Int -> Int  ∏{A: Int}" do
    --     unify (FunT IntT (VarT "A")) (FunT IntT IntT) (Tuple "A" IntT : Nil) # Assert.equal (Ok $ Tuple (FunT IntT IntT) (Tuple "A" IntT : Nil))
    --   test "✅ ∀a. a == Int  ∴  Int  ∏{a: Int}" do
    --     unify (For "a" $ VarT "a") IntT Nil # Assert.equal (Ok $ Tuple IntT (Tuple "a" IntT : Nil))
    --   test "✅ Int == ∀a. a  ∴  Int  ∏{a: Int}" do
    --     unify IntT (For "a" $ VarT "a") Nil # Assert.equal (Ok $ Tuple IntT (Tuple "a" IntT : Nil))

    -- suite "☯︎ Integer" do
    --   test "✅ 1  ∴  1 : Int" do
    --     reduce (Int 1) Nil Nil # Assert.equal (Ok $ Tuple (Int 1) IntT)

    -- suite "☯︎ Number" do
    --   test "✅ 1.1  ∴  1.1 : Num" do
    --     reduce (Num 1.1) Nil Nil # Assert.equal (Ok $ Tuple (Num 1.1) NumT)

    -- suite "☯︎ Variable" do
    --   test "❌ x  ∴  Undefined variable: x" do
    --     reduce (Var "x") Nil Nil # Assert.equal (Err $ UndefinedVar "x")
    --   test "❌ x  Γ{x: x}  ∴  Missing type: x" do
    --     reduce (Var "x") (Tuple "x" (Var "x") : Nil) Nil # Assert.equal (Err $ MissingType "x")
    --   test "✅ x  Γ{x: x : Int}  ∴  x : ∀x. x" do
    --     reduce (Var "x") (Tuple "x" (Ann (Var "x") IntT) : Nil) Nil # Assert.equal (Ok $ Tuple (Var "x") IntT)
    --   test "✅ x  Γ{y: 1, x: y}  ∴  1 : Int" do
    --     reduce (Var "x") (Tuple "y" (Int 1) : Tuple "x" (Var "y") : Nil) Nil # Assert.equal (Ok $ Tuple (Int 1) IntT)

    -- suite "☯︎ Type annotation" do
    --   test "❌ x : A  ∴  Undefined variable: x" do
    --     reduce (Ann (Var "x") (VarT "A")) Nil Nil # Assert.equal (Err $ UndefinedVar "x")
    --   test "❌ 1 : A  ∴  Undefined variable: A" do
    --     reduce (Ann (Int 1) (VarT "A")) Nil Nil # Assert.equal (Err $ UndefinedVar "A")
    --   test "❌ 1.0 : Int  ∴  Type mismatch: Num ≠ Int" do
    --     reduce (Ann (Num 1.0) IntT) Nil Nil # Assert.equal (Err $ TypeMismatch NumT IntT)
    --   test "✅ 1 : Int  ∴  1 : Int" do
    --     reduce (Ann (Int 1) IntT) Nil Nil # Assert.equal (Ok $ Tuple (Int 1) IntT)
    --   test "❌ λx. x : Int -> Num  ∴  Type mismatch: Num ≠ Int" do
    --     reduce (Ann (Lam "x" $ Var "x") (FunT IntT NumT)) Nil Nil # Assert.equal (Err $ TypeMismatch NumT IntT)
    --   test "✅ λx. x : Int -> Int  ∴  λx. x : Int -> Int" do
    --     reduce (Ann (Lam "x" $ Var "x") (FunT IntT IntT)) Nil Nil # Assert.equal (Ok $ Tuple (Lam "x" $ Var "x") (FunT IntT IntT))
    --   test "✅ λx. x : ∀a. a -> Int  ∴  λx. x : Int -> Int" do
    --     reduce (Ann (Lam "x" $ Var "x") (For "a" $ FunT (VarT "a") IntT)) Nil Nil # Assert.equal (Ok $ Tuple (Lam "x" $ Var "x") (FunT IntT IntT))
    --   test "✅ λx. x : ∀a. a -> a  ∴  λx. x : ∀a. a -> a" do
    --     reduce (Ann (Lam "x" $ Var "x") (For "a" $ FunT (VarT "a") (VarT "a"))) Nil Nil # Assert.equal (Ok $ Tuple (Lam "x" $ Var "x") (For "a" $ FunT (VarT "a") (VarT "a")))

    -- suite "☯︎ Lamda abstraction" do
    --   test "❌ λx. y  ∴  Undefined variable: y" do
    --     reduce (Lam "x" $ Var "y") Nil Nil # Assert.equal (Err $ UndefinedVar "y")
    --   test "✅ λx. 1  ∴  λx. x : ∀x. x -> Int" do
    --     reduce (Lam "x" $ Int 1) Nil Nil # Assert.equal (Ok $ Tuple (Lam "x" $ Int 1) (For "x" $ FunT (VarT "x") IntT))
    --   test "✅ λx. x  ∴  λx. x : ∀x. x -> x" do
    --     reduce (Lam "x" $ Var "x") Nil Nil # Assert.equal (Ok $ Tuple (Lam "x" $ Var "x") (For "x" $ FunT (VarT "x") (VarT "x")))

    -- suite "☯︎ Application" do
    --   test "❌ x y  ∴  Undefined variable: x" do
    --     reduce (App (Var "x") (Var "y")) Nil Nil # Assert.equal (Err $ UndefinedVar "x")
    --   test "❌ 1 2  ∴  Not a function: 1 : Int" do
    --     reduce (App (Int 1) (Int 2)) Nil Nil # Assert.equal (Err $ NotAFunction (Int 1) IntT)
    --   test "❌ x y  Γ{x: x : Int -> Num}  ∴  Undefined variable: y" do
    --     reduce (App (Var "x") (Var "y")) (Tuple "x" (Ann (Var "x") (FunT IntT NumT)) : Nil) Nil # Assert.equal (Err $ UndefinedVar "y")
    --   test "❌ x x  Γ{x: x : Int -> Num}  ∴  Type mismatch: Int ≠ Int -> Num" do
    --     reduce (App (Var "x") (Var "x")) (Tuple "x" (Ann (Var "x") (FunT IntT NumT)) : Nil) Nil # Assert.equal (Err $ TypeMismatch IntT (FunT IntT NumT))
    --   test "✅ x 1  Γ{x: x : Int -> Num}  ∴  x 1 : Num" do
    --     reduce (App (Var "x") (Int 1)) (Tuple "x" (Ann (Var "x") (FunT IntT NumT)) : Nil) Nil # Assert.equal (Ok $ Tuple (App (Var "x") (Int 1)) NumT)
    --   test "✅ x 1  Γ{x: x : ∀a. a -> a}  ∴  x 1 : Int" do
    --     reduce (App (Var "x") (Int 1)) (Tuple "x" (Ann (Var "x") (For "a" $ FunT (VarT "a") (VarT "a"))) : Nil) Nil # Assert.equal (Ok $ Tuple (App (Var "x") (Int 1)) IntT)
    --   test "✅ x 1  Γ{x: x : ∀a b. a -> b -> b}  ∴  x 1 : ∀b. b -> b" do
    --     reduce (App (Var "x") (Int 1)) (Tuple "x" (Ann (Var "x") (For "a" $ For "b" $ FunT (VarT "a") $ FunT (VarT "b") (VarT "b"))) : Nil) Nil # Assert.equal (Ok $ Tuple (App (Var "x") (Int 1)) (For "b" $ FunT (VarT "b") (VarT "b")))
    --   test "❌ (1.0 : Int -> Num) 2  ∴  Type mismatch: Int -> Num ≠ Num" do
    --     reduce (App (Ann (Num 1.0) (FunT IntT NumT)) (Int 2)) Nil Nil # Assert.equal (Err $ TypeMismatch (FunT IntT NumT) NumT)
    --   test "✅ (λx. x) 1  ∴  1 : Int" do
    --     reduce (App (Lam "x" $ Var "x") (Int 1)) Nil Nil # Assert.equal (Ok $ Tuple (Int 1) IntT)
    --   test "✅ ((λx. x) (λy. y)) 1  ∴  1 : Int" do
    --     reduce (App (App (Lam "x" (Var "x")) (Lam "y" (Var "y"))) (Int 1)) Nil Nil # Assert.equal (Ok $ Tuple (Int 1) IntT)

    -- suite "☯︎ Addition" do
    --   test "✅ (+)  ∴  (+) : ∀a. a -> a -> a" do
    --     reduce Add Nil Nil # Assert.equal (Ok $ Tuple Add (For "a" $ FunT (VarT "a") $ FunT (VarT "a") (VarT "a")))
    --   test "✅ (+) 1 2  ∴  3" do
    --     reduce (App (App Add (Int 1)) (Int 2)) Nil Nil # Assert.equal (Ok $ Tuple (Int 3) IntT)
    --   test "✅ (+) 1 2  ∴  3" do
    --     reduce (App (App Add (Num 1.0)) (Num 2.0)) Nil Nil # Assert.equal (Ok $ Tuple (Num 3.0) NumT)

    -- suite "☯︎ Subtraction" do
    --   test "✅ (-)  ∴  (-) : ∀a. a -> a -> a" do
    --     reduce Sub Nil Nil # Assert.equal (Ok $ Tuple Sub (For "a" $ FunT (VarT "a") $ FunT (VarT "a") (VarT "a")))
    --   test "✅ (-) 2 1  ∴  1" do
    --     reduce (App (App Sub (Int 2)) (Int 1)) Nil Nil # Assert.equal (Ok $ Tuple (Int 1) IntT)
    --   test "✅ (-) 2.0 1.0  ∴  1.0" do
    --     reduce (App (App Sub (Num 2.0)) (Num 1.0)) Nil Nil # Assert.equal (Ok $ Tuple (Num 1.0) NumT)

    -- suite "☯︎ Multiplication" do
    --   test "✅ (*)  ∴  (*) : ∀a. a -> a -> a" do
    --     reduce Mul Nil Nil # Assert.equal (Ok $ Tuple Mul (For "a" $ FunT (VarT "a") $ FunT (VarT "a") (VarT "a")))
    --   test "✅ (*) 2 3  ∴  6" do
    --     reduce (App (App Mul (Int 2)) (Int 3)) Nil Nil # Assert.equal (Ok $ Tuple (Int 6) IntT)
    --   test "✅ (*) 2.0 3.0  ∴  6.0" do
    --     reduce (App (App Mul (Num 2.0)) (Num 3.0)) Nil Nil # Assert.equal (Ok $ Tuple (Num 6.0) NumT)

    -- suite "☯︎ Equals" do
    --   test "✅ (==)  ∴  (==) : ∀a. a -> a -> Int" do
    --     reduce Eq Nil Nil # Assert.equal (Ok $ Tuple Eq (For "a" $ FunT (VarT "a") $ FunT (VarT "a") IntT))
    --   test "✅ (==) 2 2  ∴  1 : Int" do
    --     reduce (App (App Eq (Int 2)) (Int 2)) Nil Nil # Assert.equal (Ok $ Tuple (Int 1) IntT)
    --   test "✅ (==) 2 3  ∴  0 : Int" do
    --     reduce (App (App Eq (Int 2)) (Int 3)) Nil Nil # Assert.equal (Ok $ Tuple (Int 0) IntT)
    --   test "✅ (==) 2.0 2.0  ∴  1 : Int" do
    --     reduce (App (App Eq (Num 2.0)) (Num 2.0)) Nil Nil # Assert.equal (Ok $ Tuple (Int 1) IntT)
    --   test "✅ (==) 2.0 3.0  ∴  0 : Int" do
    --     reduce (App (App Eq (Num 2.0)) (Num 3.0)) Nil Nil # Assert.equal (Ok $ Tuple (Int 0) IntT)

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
