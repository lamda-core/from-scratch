module Test.TypedTests where

import Prelude

import Control.Monad.Free (Free)
import Dict (KV(..), dict, empty)
import Result (Result(..))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert
import Typed (Error(..), Expr(..), add2, app2, eval, mul2, sub2, unify)

typedTests :: Free TestF Unit
typedTests = 
  suite "--== Typed ==--" do
    suite "☯︎ unify" do
      test "❌ Type == Int  ∴  Type mismatch: Type ≠ Int" do
        unify Typ IntT empty # Assert.equal (Err $ TypeMismatch Typ IntT)
      test "✅ Type == Type  ∴  Type" do
        unify Typ Typ empty # Assert.equal (Ok $ Typ `KV` empty)

      test "✅ _ == x  Γ{x: Int}  ∴  Int" do
        unify Any (Var "x") (dict ["x" `KV` IntT]) # Assert.equal (Ok $ IntT `KV` (dict ["x" `KV` IntT]))
      test "✅ x == _  Γ{x: Int}  ∴  Int" do
        unify (Var "x") Any (dict ["x" `KV` IntT]) # Assert.equal (Ok $ IntT `KV` (dict ["x" `KV` IntT]))
      test "✅ x == y  Γ{y: Int}  ∴  Int  Γ{y: Int, x: Int}" do
        unify (Var "x") (Var "y") (dict ["y" `KV` IntT]) # Assert.equal (Ok $ IntT `KV` dict ["y" `KV` IntT, "x" `KV` IntT])
      test "✅ x == y  Γ{x: Int}  ∴  Int  Γ{x: Int, y: Int}" do
        unify (Var "x") (Var "y") (dict ["x" `KV` IntT]) # Assert.equal (Ok $ IntT `KV` dict ["x" `KV` IntT, "y" `KV` IntT])
      test "✅ x == y  Γ{x: x, y: Int}  ∴  Int  Γ{x: Int, y: Int}" do
        unify (Var "x") (Var "y") (dict ["x" `KV` Var "x", "y" `KV` IntT]) # Assert.equal (Ok $ IntT `KV` dict ["x" `KV` IntT, "y" `KV` IntT])
      test "✅ x == y  Γ{x: Int, y: y}  ∴  Int  Γ{x: Int, y: Int}" do
        unify (Var "x") (Var "y") (dict ["x" `KV` IntT, "y" `KV` Var "y"]) # Assert.equal (Ok $ IntT `KV` dict ["x" `KV` IntT, "y" `KV` IntT])
      test "✅ Int == x  ∴  Int  Γ{x: Int}" do
        unify IntT (Var "x") empty # Assert.equal (Ok $ IntT `KV` dict ["x" `KV` IntT])
      test "✅ Int == x  Γ{x: x}  ∴  Int  Γ{x: Int}" do
        unify IntT (Var "x") (dict ["x" `KV` Var "x"]) # Assert.equal (Ok $ IntT `KV` dict ["x" `KV` IntT])
      test "❌ Int == x  Γ{x: Type}  ∴  Int mismatch: Int ≠ Type" do
        unify IntT (Var "x") (dict ["x" `KV` Typ]) # Assert.equal (Err $ TypeMismatch IntT Typ)
      test "✅ _@x == 1  ∴  1  Γ{x: 1}" do
        unify (Any `As` "x") (Int 1) empty # Assert.equal (Ok $ Int 1 `KV` (dict ["x" `KV` Int 1]))
      test "✅ 1 == _@x  ∴  1  Γ{x: 1}" do
        unify (Int 1) (Any `As` "x") empty # Assert.equal (Ok $ Int 1 `KV` (dict ["x" `KV` Int 1]))
      test "❌ (x : Int) == Type  ∴  Type mismatch: Type ≠ Int" do
        unify (Var "x" `Ann` IntT) Typ empty # Assert.equal (Err $ TypeMismatch Typ IntT)
      test "✅ (_ : x) == 1  ∴  1  Γ{x: Int}" do
        unify (Any `Ann` Var "x") (Int 1) empty # Assert.equal (Ok $ Int 1 `KV` (dict ["x" `KV` IntT]))
      test "❌ Type == (x : Int)  ∴  Type mismatch: Type ≠ Int" do
        unify Typ (Var "x" `Ann` IntT) empty # Assert.equal (Err $ TypeMismatch Typ IntT)
      test "✅ 1 == (_ : x)  ∴  1  Γ{x: Int}" do
        unify (Int 1) (Any `Ann` Var "x") empty # Assert.equal (Ok $ Int 1 `KV` (dict ["x" `KV` IntT]))
      test "❌ x -> x == Type -> Int  ∴  Type mismatch: Type ≠ Int" do
        unify (Var "x" `To` Var "x") (Typ `To` IntT) empty # Assert.equal (Err $ TypeMismatch Typ IntT)
      test "✅ x -> y == Type -> Int  ∴  Type -> Int  Γ{x: Type, y: Int}" do
        unify (Var "x" `To` Var "y") (Typ `To` IntT) empty # Assert.equal (Ok $ (Typ `To` IntT) `KV` (dict ["x" `KV` Typ, "y" `KV` IntT]))
      test "❌ x -> Int == Type -> x  ∴  Type mismatch: Int ≠ Type" do
        unify (Var "x" `To` IntT) (Typ `To` Var "x") empty # Assert.equal (Err $ TypeMismatch IntT Typ)
      test "✅ x -> Int == Type -> y  ∴  Type -> Int  Γ{x: Type, y: Int}" do
        unify (Var "x" `To` IntT) (Typ `To` Var "y") empty # Assert.equal (Ok $ (Typ `To` IntT) `KV` dict ["x" `KV` Typ, "y" `KV` IntT])
      test "✅ Type | Int == Type  ∴  Type" do
        unify (Typ `Or` IntT) Typ empty # Assert.equal (Ok $ Typ `KV` empty)
      test "✅ Type | Int == Int  ∴  Int" do
        unify (Typ `Or` IntT) IntT empty # Assert.equal (Ok $ IntT `KV` empty)
      test "✅ Type == Type | Int  ∴  Type" do
        unify Typ (Typ `Or` IntT) empty # Assert.equal (Ok $ Typ `KV` empty)
      test "✅ Int == Type | Int  ∴  Int" do
        unify IntT (Typ `Or` IntT) empty # Assert.equal (Ok $ IntT `KV` empty)
      test "❌ (x, x) == (Type, Int)  ∴  Type mismatch: Type ≠ Int" do
        unify (Var "x" `And` Var "x") (Typ `And` IntT) empty # Assert.equal (Err $ TypeMismatch Typ IntT)
      test "✅ (x, y) == (Type, Int)  ∴  (Type, Int)  Γ{x: Type, y: Int}" do
        unify (Var "x" `And` Var "y") (Typ `And` IntT) empty # Assert.equal (Ok $ (Typ `And` IntT) `KV` (dict ["x" `KV` Typ, "y" `KV` IntT]))
      test "❌ (x, Int) == (Type, x)  ∴  Type mismatch: Int ≠ Type" do
        unify (Var "x" `And` IntT) (Typ `And` Var "x") empty # Assert.equal (Err $ TypeMismatch IntT Typ)
      test "✅ (x, Int) == (Type, y)  ∴  (Type, Int)  Γ{x: Type, y: Int}" do
        unify (Var "x" `And` IntT) (Typ `And` Var "y") empty # Assert.equal (Ok $ (Typ `And` IntT) `KV` dict ["x" `KV` Typ, "y" `KV` IntT])
      test "❌ x Int == Type x  ∴  Type mismatch: Int ≠ Type" do
        unify (Var "x" `App` IntT) (Typ `App` Var "x") empty # Assert.equal (Err $ TypeMismatch IntT Typ)
      test "✅ x Int == Type y  ∴  Type Int  Γ{x: Type, y: Int}" do
        unify (Var "x" `App` IntT) (Typ `App` Var "y") empty # Assert.equal (Ok $ (Typ `App` IntT) `KV` dict ["x" `KV` Typ, "y" `KV` IntT])

    suite "☯︎ eval" do
      test "✅ _  ∴  _ : _" do
        eval Any empty # Assert.equal (Ok $ Any `KV` Any)
      test "✅ Type ∴  Type : Type" do
        eval Typ empty # Assert.equal (Ok $ Typ `KV` Typ)
      test "✅ Int  ∴  Int : Type" do
        eval IntT empty # Assert.equal (Ok $ IntT `KV` Typ)
      test "✅ 42  ∴  42 : Int" do
        eval (Int 42) empty # Assert.equal (Ok $ Int 42 `KV` IntT)

      test "❌ A  ∴  Undefined name: A" do
        eval (Ctr "A") empty # Assert.equal (Err $ UndefinedName "A")
      test "✅ A  Γ{A: A : Type}  ∴  A : Type" do
        eval (Ctr "A") (dict ["A" `KV` (Ctr "A" `Ann` Typ)]) # Assert.equal (Ok $ Ctr "A" `KV` Typ)

      test "❌ x  ∴  Undefined name: x" do
        eval (Var "x") empty # Assert.equal (Err $ UndefinedName "x")
      test "✅ x  Γ{x: x}  ∴  x : x" do
        eval (Var "x") (dict ["x" `KV` Var "x"]) # Assert.equal (Ok $ Var "x" `KV` Var "x")
      test "❌ x  Γ{x: x : y}  ∴  Undefined name: y" do
        eval (Var "x") (dict ["x" `KV` (Var "x" `Ann` Var "y")]) # Assert.equal (Err $ UndefinedName "y")
      test "✅ x  Γ{x: x : Int}  ∴  x : Int" do
        eval (Var "x") (dict ["x" `KV` (Var "x" `Ann` IntT)]) # Assert.equal (Ok $ Var "x" `KV` IntT)
      -- test "✅ x  Γ{x: x -> x}  ∴  x -> x" do
      --   eval (Var "x") (dict ["x" `KV` (Var "x" `To` Var "x")]) # Assert.equal (Ok $ Var "x" `To` Var "x")
    --   test "✅ x  Γ{x: y -> x y}  ∴  (y -> x y) @ x" do
    --     eval (Var "x") (dict ["x" `KV` (Var "y" `To` (Var "x" `App` Var "y"))]) # Assert.equal (Ok $ (Var "y" `To` (Var "x" `App` Var "y")) `As` "x")

      test "✅ 1 @ x  ∴  1 : Int" do
        eval (Int 1 `As` "x") empty # Assert.equal (Ok $ Int 1 `KV` IntT)
      -- test "✅ (y -> x y) @ x  ∴  (y -> x) @ x" do
      --   eval ((Var "y" `To` (Var "x" `App` Var "y")) `As` "x") empty # Assert.equal (Ok $ (Var "y" `To` (Var "x" `App` Var "y")) `As` "x")

      test "❌ 1 : Type  ∴  Type mismatch: Int ≠ Type" do
        eval (Int 1 `Ann` Typ) empty # Assert.equal (Err $ TypeMismatch IntT Typ)
      test "✅ 1 : _  ∴  1 : Int" do
        eval (Int 1 `Ann` Any) empty # Assert.equal (Ok $ Int 1 `KV` IntT)

      test "❌ x -> y  ∴  Undefined name: y" do
        eval (Var "x" `To` Var "y") empty # Assert.equal (Err $ UndefinedName "y")
      test "✅ x -> x  ∴  x -> x : x -> x" do
        eval (Var "x" `To` Var "x") empty # Assert.equal (Ok $ (Var "x" `To` Var "x") `KV` (Var "x" `To` Var "x"))

      test "❌ 1 | Type  ∴  Type mismatch: Int ≠ Type" do
        eval (Int 1 `Or` Typ) empty # Assert.equal (Err $ TypeMismatch IntT Typ)
      test "✅ 1 | y  Γ{y: y}  ∴  (1 | y) : Int" do
        eval (Int 1 `Or` Var "y") (dict ["y" `KV` Var "y"]) # Assert.equal (Ok $ (Int 1 `Or` Var "y") `KV` IntT)
      -- TODO: test for missing cases
      -- TODO: test for redundant cases

      test "✅ (1, Type)  ∴  (1, Type) : (Int, Type)" do
        eval (Int 1 `And` Typ) empty # Assert.equal (Ok $ (Int 1 `And` Typ) `KV` (IntT `And` Typ))

      test "❌ 1 Type  ∴  Type mismatch: Int ≠ Type -> _" do
        eval (Int 1 `App` Typ) empty # Assert.equal (Err $ TypeMismatch IntT (Typ `To` Any))
      test "❌ (_ -> 1) x  ∴  Undefined name: x" do
        eval ((Any `To` Int 1) `App` Var "x") empty # Assert.equal (Err $ UndefinedName "x")
      test "✅ (1 -> _) 2  ∴  Pattern mismatch: 1 ≠ 2" do
        eval ((Int 1 `To` Any) `App` Int 2) empty # Assert.equal (Err $ PatternMismatch (Int 1) (Int 2))
      test "✅ (x -> x) 1  ∴  1 : Int" do
        eval ((Var "x" `To` Var "x") `App` Int 1) empty # Assert.equal (Ok $ Int 1 `KV` IntT)
      test "✅ (1 -> 2 | x -> x) 1  ∴  2 : Int" do
        eval (((Int 1 `To` Int 2) `Or` (Var "x" `To` Var "x")) `App` Int 1) empty # Assert.equal (Ok $ Int 2 `KV` IntT)
      test "✅ (1 -> 2 | x -> x) 3  ∴  3 : Int" do
        eval (((Int 1 `To` Int 2) `Or` (Var "x" `To` Var "x")) `App` Int 3) empty # Assert.equal (Ok $ Int 3 `KV` IntT)
      test "✅ (_ -> 1, x -> x) 2  ∴  (1, 2) : (Int, Int)" do
        eval (((Any `To` Int 1) `And` (Var "x" `To` Var "x")) `App` Int 2) empty # Assert.equal (Ok $ (Int 1 `And` Int 2) `KV` (IntT `And` IntT))

    suite "☯︎ eval Add" do
      test "✅ (+)  ∴  (+)" do
        eval Add empty # Assert.equal (Ok $ Add `KV` (Var "a" `To` (Var "a" `To` Var "a")))
      test "✅ (+) 1 2  ∴  3" do
        eval (App (App Add (Int 1)) (Int 2)) empty # Assert.equal (Ok $ Int 3 `KV` IntT)

    suite "☯︎ eval Sub" do
      test "✅ (-)  ∴  (-)" do
        eval Sub empty # Assert.equal (Ok $ Sub `KV` (Var "a" `To` (Var "a" `To` Var "a")))
      test "✅ (-) 2 1  ∴  1" do
        eval (App (App Sub (Int 2)) (Int 1)) empty # Assert.equal (Ok $ Int 1 `KV` IntT)

    suite "☯︎ eval Mul" do
      test "✅ (*)  ∴  (*)" do
        eval Mul empty # Assert.equal (Ok $ Mul `KV` (Var "a" `To` (Var "a" `To` Var "a")))
      test "✅ (*) 2 3  ∴  6" do
        eval (App (App Mul (Int 2)) (Int 3)) empty # Assert.equal (Ok $ Int 6 `KV` IntT)

    -- suite "☯︎ Factorial" do
    --   test "✅ f  Γ{f: factorial}  ∴  factorial @ f" do
    --     eval (Var "f") (dict [KV "f" factorial]) # Assert.equal (Ok $ factorial `As` "f")
    --   test "✅ f 0  Γ{f: factorial}  ∴  (factorial @ f) 1" do
    --     eval (Var "f" `App` Int 0) (dict [KV "f" factorial]) # Assert.equal (Ok $ Int 1)
    --   test "✅ f 1  Γ{f: factorial}  ∴  1" do
    --     eval (Var "f" `App` Int 1) (dict [KV "f" factorial]) # Assert.equal (Ok $ Int 1)
    --   test "✅ f 2  Γ{f: factorial}  ∴  2" do
    --     eval (Var "f" `App` Int 2) (dict [KV "f" factorial]) # Assert.equal (Ok $ Int 2)
    --   test "✅ f 5  Γ{f: factorial}  ∴  120" do
    --     eval (Var "f" `App` Int 5) (dict [KV "f" factorial]) # Assert.equal (Ok $ Int 120)

    -- suite "☯︎ Ackermann" do
    --   test "✅ a  Γ{a: ackermann}  ∴  ackermann @ a" do
    --     eval (Var "a") (dict [KV "a" ackermann]) # Assert.equal (Ok $ ackermann `As` "a")
    --   test "✅ a 0  Γ{a: ackermann}  ∴  n -> n + 1" do
    --     eval (App (Var "a") (Int 0)) (dict [KV "a" ackermann]) # Assert.equal (Ok $ Var "n" `To` (Var "n" `add2` Int 1))
    --   test "✅ a 0 0  Γ{a: ackermann}  ∴  1" do
    --     eval (app2 (Var "a") (Int 0) (Int 0)) (dict [KV "a" ackermann]) # Assert.equal (Ok $ Int 1)
    --   test "✅ a 0 0  Γ{a: ackermann}  ∴  1" do
    --     eval (app2 (Var "a") (Int 0) (Int 0)) (dict [KV "a" ackermann]) # Assert.equal (Ok $ Int 1)
    --   test "✅ a 1 1  Γ{a: ackermann}  ∴  3" do
    --     eval (app2 (Var "a") (Int 1) (Int 1)) (dict [KV "a" ackermann]) # Assert.equal (Ok $ Int 3)
    --   test "✅ a 2 2  Γ{a: ackermann}  ∴  7" do
    --     eval (app2 (Var "a") (Int 2) (Int 2)) (dict [KV "a" ackermann]) # Assert.equal (Ok $ Int 7)
    --   test "✅ a 3 2  Γ{a: ackermann}  ∴  29" do
    --     eval (app2 (Var "a") (Int 3) (Int 2)) (dict [KV "a" ackermann]) # Assert.equal (Ok $ Int 29)

    -- where
    --   -- f 0 = 1
    --   -- f n = n * f (n - 1)
    --   factorial =
    --     ( Int 0 `To`
    --         Int 1
    --     ) `Or` (Var "n" `To`
    --         (Var "n" `mul2` (Var "f" `App` (Var "n" `sub2` Int 1)))
    --     )

    --   -- a 0 n = n + 1
    --   -- a m 0 = a (m-1) 1
    --   -- a m n = a (m-1) (a m (n-1))
    --   ackermann =
    --     ( Int 0 `To` (Var "n" `To`
    --         (Var "n" `add2` Int 1))
    --     ) `Or` (Var "m" `To` (Int 0 `To`
    --         (app2 (Var "a") (Var "m" `sub2` Int 1) (Int 1)))
    --     ) `Or` (Var "m" `To` (Var "n" `To`
    --       (app2 (Var "a") (Var "m" `sub2` Int 1) (app2 (Var "a") (Var "m") (Var "n" `sub2` Int 1))))
    --     )
