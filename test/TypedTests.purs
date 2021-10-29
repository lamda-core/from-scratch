module Test.TypedTests where

import Prelude

import Control.Monad.Free (Free)
import Dict (Dict(..), KV(..), empty)
import Result (Result(..))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert
import Typed (Error(..), Expr(..), add2, app2, eval, match, mul2, sub2)

typedTests :: Free TestF Unit
typedTests = 
  suite "--== Typed ==--" do
    -- suite "☯︎ match" do
    --   test "❌ _ <- y  ∴  Undefined variable: y" do
    --     match Any (Var "y") empty # Assert.equal (Err $ UndefinedName "y")
    --   test "✅ _ <- B  ∴  Γ{}" do
    --     match Any (Ctr "B") empty # Assert.equal (Ok empty)
    --   test "✅ 1 <- x  ∴  Γ{x: x}" do
    --     match (Int 1) (Var "x") (Dict ["x" `KV` Var "x"]) # Assert.equal (Ok $ Dict ["x" `KV` Var "x"])
    --   test "✅ 1 <- 1  ∴  Γ{}" do
    --     match (Int 1) (Int 1) empty # Assert.equal (Ok empty)
    --   test "❌ 1 <- 2  ∴  Pattern mismatch: 1 ≠ 2" do
    --     match (Int 1) (Int 2) empty # Assert.equal (Err $ PatternMismatch (Int 1) (Int 2))
    --   test "✅ A <- A  ∴  Γ{}" do
    --     match (Ctr "A") (Ctr "A") empty # Assert.equal (Ok empty)
    --   test "❌ A <- B  ∴  Pattern mismatch: A ≠ B" do
    --     match (Ctr "A") (Ctr "B") empty # Assert.equal (Err $ PatternMismatch (Ctr "A") (Ctr "B"))
    --   test "✅ x <- B  Γ{x: A}  ∴  {x: B}" do
    --     match (Var "x") (Ctr "B") (Dict ["x" `KV` Ctr "A"]) # Assert.equal (Ok $ Dict ["x" `KV` Ctr "B"])
    --   test "✅ (x -> y) <- (A -> B)  ∴  Γ{x: A, y: B}" do
    --     match (Var "x" `To` Var "y") (Ctr "A" `To` Ctr "B") empty # Assert.equal (Ok $ Dict ["x" `KV` Ctr "A", "y" `KV` Ctr "B"])
    --   test "✅ x | y <- A  ∴  Γ{x: A}" do
    --     match (Var "x" `Or` Var "y") (Ctr "A") empty # Assert.equal (Ok $ Dict ["x" `KV` Ctr "A"])
    --   test "✅ A | y <- B  ∴  Γ{y: B}" do
    --     match (Ctr "A" `Or` Var "y") (Ctr "B") empty # Assert.equal (Ok $ Dict ["y" `KV` Ctr "B"])
    --   test "✅ (x, y) <- (A, B)  ∴  Γ{x: A, y: B}" do
    --     match (Var "x" `And` Var "y") (Ctr "A" `And` Ctr "B") empty # Assert.equal (Ok $ Dict ["x" `KV` Ctr "A", "y" `KV` Ctr "B"])
    --   test "✅ x y <- A B  ∴  Γ{x: A, y: B}" do
    --     match (Var "x" `App` Var "y") (Ctr "A" `App` Ctr "B") empty # Assert.equal (Ok $ Dict ["x" `KV` Ctr "A", "y" `KV` Ctr "B"])

    suite "☯︎ eval Any" do
      test "✅ _  ∴  _ : _" do
        eval Any empty # Assert.equal (Ok $ Any `KV` Any)

    suite "☯︎ eval TypT" do
      test "✅ Type ∴  Type : Type" do
        eval Typ empty # Assert.equal (Ok $ Typ `KV` Typ)

    suite "☯︎ eval IntT" do
      test "✅ Int  ∴  Int : Type" do
        eval IntT empty # Assert.equal (Ok $ IntT `KV` Typ)

    suite "☯︎ eval Int" do
      test "✅ 42  ∴  42 : Int" do
        eval (Int 42) empty # Assert.equal (Ok $ Int 42 `KV` IntT)

    suite "☯︎ eval Var" do
      test "❌ x  ∴  Undefined name: x" do
        eval (Var "x") empty # Assert.equal (Err $ UndefinedName "x")
      test "✅ x  Γ{x: x}  ∴  x : x" do
        eval (Var "x") (Dict ["x" `KV` Var "x"]) # Assert.equal (Ok $ Var "x" `KV` Var "x")
      -- test "✅ x  Γ{x: x -> x}  ∴  x -> x" do
      --   eval (Var "x") (Dict ["x" `KV` (Var "x" `To` Var "x")]) # Assert.equal (Ok $ Var "x" `To` Var "x")
    --   test "✅ x  Γ{x: y -> x y}  ∴  (y -> x y) @ x" do
    --     eval (Var "x") (Dict ["x" `KV` (Var "y" `To` (Var "x" `App` Var "y"))]) # Assert.equal (Ok $ (Var "y" `To` (Var "x" `App` Var "y")) `As` "x")

    suite "☯︎ eval Ctr" do
      test "❌ A  ∴  Undefined name: A" do
        eval (Ctr "A") empty # Assert.equal (Err $ UndefinedName "A")
      test "❌ A : x  ∴  Undefined name: x" do
        eval (Ctr "A" `Ann` Var "x") empty # Assert.equal (Err $ UndefinedName "x")
      test "✅ A : Type  ∴  A : Type" do
        eval (Ctr "A" `Ann` Typ) empty # Assert.equal (Ok $ Ctr "A" `KV` Typ)

    suite "☯︎ eval To" do
      test "❌ x -> y  ∴  Undefined name: y" do
        eval (Var "x" `To` Var "y") empty # Assert.equal (Err $ UndefinedName "y")
      test "✅ x -> x  ∴  x -> x : x -> x" do
        eval (Var "x" `To` Var "x") empty # Assert.equal (Ok $ (Var "x" `To` Var "x") `KV` (Var "x" `To` Var "x"))

    suite "☯︎ eval Or" do
      test "❌ 1 | Type  ∴  Type mismatch: Int ≠ Type" do
        eval (Int 1 `Or` Typ) empty # Assert.equal (Err $ TypeMismatch IntT Typ)
      test "✅ 1 | y  Γ{y: y}  ∴  (1 | y) : Int" do
        eval (Int 1 `Or` Var "y") (Dict ["y" `KV` Var "y"]) # Assert.equal (Ok $ (Int 1 `Or` Var "y") `KV` IntT)

    suite "☯︎ eval And" do
      test "✅ (1, Type)  ∴  (1, Type) : (Int, Type)" do
        eval (Int 1 `And` Typ) empty # Assert.equal (Ok $ (Int 1 `And` Typ) `KV` (IntT `And` Typ))

    -- suite "☯︎ eval App" do
    --   test "❌ _ B  ∴  Not a value: _" do
    --     eval (Any `App` Ctr "B") empty # Assert.equal (Err $ NotAValue Any)
    --   test "❌ A _  ∴  Not a value: _" do
    --     eval (Ctr "A" `App` Any) empty # Assert.equal (Err $ NotAValue Any)
    --   test "❌ 1 B  ∴  Not a function: 1" do
    --     eval (Int 1 `App` Ctr "B") empty # Assert.equal (Err $ NotAFunction (Int 1))
    --   test "✅ A B  ∴  A B" do
    --     eval (Ctr "A" `App` Ctr "B") empty # Assert.equal (Ok $ Ctr "A" `App` Ctr "B")
    --   test "✅ x B  Γ{x: x}  ∴  x B" do
    --     eval (Var "x" `App` Ctr "B") (Dict ["x" `KV` Var "x"]) # Assert.equal (Ok $ Var "x" `App` Ctr "B")
    --   test "✅ (A -> B) x  Γ{x: x}  ∴  (A -> A) x" do
    --     eval (Ctr "A" `To` Ctr "B" `App` Var "x") (Dict ["x" `KV` Var "x"]) # Assert.equal (Ok $ (Ctr "A" `To` Ctr "B") `App` Var "x")
    --   test "❌ (A -> B) C  ∴  Pattern mismatch: A ≠ C" do
    --     eval (Ctr "A" `To` Ctr "B" `App` Ctr "C") empty # Assert.equal (Err $ PatternMismatch (Ctr "A") (Ctr "C"))
    --   test "✅ (A -> B) A  ∴  B" do
    --     eval (Ctr "A" `To` Ctr "B" `App` Ctr "A") empty # Assert.equal (Ok $ Ctr "B")
    --   test "✅ (A -> C | B -> D) A  ∴  C" do
    --     eval ((Ctr "A" `To` Ctr "C") `Or` (Ctr "B" `To` Ctr "D") `App` Ctr "A") empty # Assert.equal (Ok $ Ctr "C")
    --   test "✅ (A -> C | B -> D) B  ∴  C" do
    --     eval ((Ctr "A" `To` Ctr "C") `Or` (Ctr "B" `To` Ctr "D") `App` Ctr "B") empty # Assert.equal (Ok $ Ctr "D")
    --   test "❌ (A -> C | B -> D) C  ∴  Pattern mismatch: B ≠ C" do
    --     eval ((Ctr "A" `To` Ctr "C") `Or` (Ctr "B" `To` Ctr "D") `App` Ctr "C") empty # Assert.equal (Err $ PatternMismatch (Ctr "B") (Ctr "C"))
    --   test "✅ (A, B) C  ∴  (A C, B C)" do
    --     eval ((Ctr "A" `And` Ctr "B") `App` Ctr "C") empty # Assert.equal (Ok $ Ctr "A" `App` Ctr "C" `And` (Ctr "B" `App` Ctr "C"))
    --   test "✅ A B C  ∴  A B C" do
    --     eval (Ctr "A" `App` Ctr "B" `App` Ctr "C") empty # Assert.equal (Ok $ Ctr "A" `App` Ctr "B" `App` Ctr "C")

    suite "☯︎ eval As" do
      test "✅ 1 @ x  ∴  1 : Int" do
        eval (Int 1 `As` "x") empty # Assert.equal (Ok $ Int 1 `KV` IntT)
      -- test "✅ (y -> x y) @ x  ∴  (y -> x) @ x" do
      --   eval ((Var "y" `To` (Var "x" `App` Var "y")) `As` "x") empty # Assert.equal (Ok $ (Var "y" `To` (Var "x" `App` Var "y")) `As` "x")

    -- suite "☯︎ eval Add" do
    --   test "✅ (+)  ∴  (+)" do
    --     eval Add empty # Assert.equal (Ok Add)
    --   test "✅ (+) 1 2  ∴  3" do
    --     eval (App (App Add (Int 1)) (Int 2)) empty # Assert.equal (Ok $ Int 3)

    -- suite "☯︎ eval Sub" do
    --   test "✅ (-)  ∴  (-)" do
    --     eval Sub empty # Assert.equal (Ok Sub)
    --   test "✅ (-) 2 1  ∴  1" do
    --     eval (App (App Sub (Int 2)) (Int 1)) empty # Assert.equal (Ok $ Int 1)

    -- suite "☯︎ eval Mul" do
    --   test "✅ (*)  ∴  (*)" do
    --     eval Mul empty # Assert.equal (Ok Mul)
    --   test "✅ (*) 2 3  ∴  6" do
    --     eval (App (App Mul (Int 2)) (Int 3)) empty # Assert.equal (Ok $ Int 6)

    -- suite "☯︎ Factorial" do
    --   test "✅ f  Γ{f: factorial}  ∴  factorial @ f" do
    --     eval (Var "f") (Dict [KV "f" factorial]) # Assert.equal (Ok $ factorial `As` "f")
    --   test "✅ f 0  Γ{f: factorial}  ∴  (factorial @ f) 1" do
    --     eval (Var "f" `App` Int 0) (Dict [KV "f" factorial]) # Assert.equal (Ok $ Int 1)
    --   test "✅ f 1  Γ{f: factorial}  ∴  1" do
    --     eval (Var "f" `App` Int 1) (Dict [KV "f" factorial]) # Assert.equal (Ok $ Int 1)
    --   test "✅ f 2  Γ{f: factorial}  ∴  2" do
    --     eval (Var "f" `App` Int 2) (Dict [KV "f" factorial]) # Assert.equal (Ok $ Int 2)
    --   test "✅ f 5  Γ{f: factorial}  ∴  120" do
    --     eval (Var "f" `App` Int 5) (Dict [KV "f" factorial]) # Assert.equal (Ok $ Int 120)

    -- suite "☯︎ Ackermann" do
    --   test "✅ a  Γ{a: ackermann}  ∴  ackermann @ a" do
    --     eval (Var "a") (Dict [KV "a" ackermann]) # Assert.equal (Ok $ ackermann `As` "a")
    --   test "✅ a 0  Γ{a: ackermann}  ∴  n -> n + 1" do
    --     eval (App (Var "a") (Int 0)) (Dict [KV "a" ackermann]) # Assert.equal (Ok $ Var "n" `To` (Var "n" `add2` Int 1))
    --   test "✅ a 0 0  Γ{a: ackermann}  ∴  1" do
    --     eval (app2 (Var "a") (Int 0) (Int 0)) (Dict [KV "a" ackermann]) # Assert.equal (Ok $ Int 1)
    --   test "✅ a 0 0  Γ{a: ackermann}  ∴  1" do
    --     eval (app2 (Var "a") (Int 0) (Int 0)) (Dict [KV "a" ackermann]) # Assert.equal (Ok $ Int 1)
    --   test "✅ a 1 1  Γ{a: ackermann}  ∴  3" do
    --     eval (app2 (Var "a") (Int 1) (Int 1)) (Dict [KV "a" ackermann]) # Assert.equal (Ok $ Int 3)
    --   test "✅ a 2 2  Γ{a: ackermann}  ∴  7" do
    --     eval (app2 (Var "a") (Int 2) (Int 2)) (Dict [KV "a" ackermann]) # Assert.equal (Ok $ Int 7)
    --   test "✅ a 3 2  Γ{a: ackermann}  ∴  29" do
    --     eval (app2 (Var "a") (Int 3) (Int 2)) (Dict [KV "a" ackermann]) # Assert.equal (Ok $ Int 29)

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
