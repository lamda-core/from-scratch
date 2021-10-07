import Test.Hspec
import Typed

main :: IO ()
main = hspec $ do
  describe "---=== Typed ===---" $ do
    describe "☯︎ Number" $ do
      it "✅ 1  ∴  1 : Num" $ do
        reduce (Num 1) [] `shouldBe` Ok (Num 1, NumT)
      it "❌ 1 : Type  ∴  Type mismatch: 1 : Type" $ do
        reduce (Ann (Num 1) TypT) [] `shouldBe` Err (TypeMismatch (Num 1) TypT NumT)
      it "✅ 1 : Num  ∴  1 : Num" $ do
        reduce (Ann (Num 1) NumT) [] `shouldBe` Ok (Num 1, NumT)

    describe "☯︎ Variable" $ do
      it "❌ x  ∴  Undefined variable: x" $ do
        reduce (Var "x") [] `shouldBe` Err (UndefinedVar "x")
      it "❌ x : Num  ∴  Undefined variable: x" $ do
        reduce (Ann (Var "x") NumT) [] `shouldBe` Err (UndefinedVar "x")
      it "✅ x  Γ{x : Num = x}  ∴  x : Num" $ do
        reduce (Var "x") [("x", (Var "x", NumT))] `shouldBe` Ok (Var "x", NumT)
      it "❌ x : Type  Γ{x : Num = x}  ∴  Type mismatch: (x : Num) : Type" $ do
        reduce (Ann (Var "x") TypT) [("x", (Var "x", NumT))] `shouldBe` Err (TypeMismatch (Var "x") TypT NumT)
      it "✅ x : Num  Γ{x : Num = x}  ∴  x : Num" $ do
        reduce (Ann (Var "x") NumT) [("x", (Var "x", NumT))] `shouldBe` Ok (Var "x", NumT)
      it "✅ x  Γ{y : Num = 1, x: Num = y}  ∴  1 : Num" $ do
        reduce (Var "x") [("y", (Num 1, NumT)), ("x", (Var "y", NumT))] `shouldBe` Ok (Num 1, NumT)

    describe "☯︎ λ Lamda abstraction" $ do
      it "❌ λx. x  ∴  Missing type: x" $ do
        reduce (Lam "x" (Var "x")) [] `shouldBe` Err (MissingType "x")
      it "❌ λx. x : Num  ∴  Type mismatch: (x : Num): Type" $ do
        reduce (Ann (Lam "x" (Var "x")) NumT) [] `shouldBe` Err (MissingType "x")
      it "✅ λx. x : Num -> Num  ∴  (λx. x) : Num -> Num" $ do
        reduce (Ann (Lam "x" (Var "x")) (FunT NumT NumT)) [] `shouldBe` Ok (Lam "x" (Var "x"), FunT NumT NumT)
      it "❌ λx. x : Num -> Type  ∴  Type mismatch: (x : Num): Type" $ do
        reduce (Ann (Lam "x" (Var "x")) (FunT NumT TypT)) [] `shouldBe` Err (TypeMismatch (Var "x") TypT NumT)
      it "✅ λx. x : Type -> Type ∴  (λx. x) : Type -> Type" $ do
        reduce (Ann (Lam "x" (Var "x")) (FunT TypT TypT)) [] `shouldBe` Ok (Lam "x" (Var "x"), FunT TypT TypT)

--   describe "☯︎ Application" $ do
--     it "1 2  ∴  Not a function" $ do
--       reduce (App (Num 1) (Num 2)) [] `shouldBe` Err (NotAFunction (Num 1))
--     it "f x  ∴  Undefined variable: f" $ do
--       reduce (App (Var "f") (Var "x")) [] `shouldBe` Err (UndefinedVar "f")
--     it "f x  Γ{f: f}  ∴  Undefined variable: x" $ do
--       reduce (App (Var "f") (Var "x")) [("f", Var "f")] `shouldBe` Err (UndefinedVar "x")
--     it "f 1  Γ{f: f}  ∴  f 1" $ do
--       reduce (App (Var "f") (Num 1)) [("f", Var "f")] `shouldBe` Ok (App (Var "f") (Num 1))
--     it "(λx. 1) 2  ∴  1" $ do
--       reduce (App (Lam "x" (Num 1)) (Num 2)) [] `shouldBe` Ok (Num 1)
--     it "(λx. x) 1  ∴  1" $ do
--       reduce (App (Lam "x" (Var "x")) (Num 1)) [] `shouldBe` Ok (Num 1)
--     it "((λx. x) (λy. y)) 1  ∴  1" $ do
--       reduce (App (App (Lam "x" (Var "x")) (Lam "y" (Var "y"))) (Num 1)) [] `shouldBe` Ok (Num 1)

--   describe "☯︎ Addition" $ do
--     it "(+)  ∴  (+)" $ do
--       reduce Add [] `shouldBe` Ok Add
--     it "(+) x  Γ{x: x}  ∴  (+) x" $ do
--       reduce (App Add (Var "x")) [("x", Var "x")] `shouldBe` Ok (App Add (Var "x"))
--     it "(+) x y  Γ{x: x, y: y}  ∴  x + y" $ do
--       reduce (add (Var "x") (Var "y")) [("x", Var "x"), ("y", Var "y")] `shouldBe` Ok (add (Var "x") (Var "y"))
--     it "(+) x y  Γ{x: 1, y: 2}  ∴  3" $ do
--       reduce (add (Var "x") (Var "y")) [("x", Num 1), ("y", Num 2)] `shouldBe` Ok (Num 3)

--   describe "☯︎ Subtraction" $ do
--     it "(-)  ∴  (-)" $ do
--       reduce Sub [] `shouldBe` Ok Sub
--     it "(-) x  Γ{x: x}  ∴  (-) x" $ do
--       reduce (App Sub (Var "x")) [("x", Var "x")] `shouldBe` Ok (App Sub (Var "x"))
--     it "(-) x y  Γ{x: x, y: y}  ∴  x - y" $ do
--       reduce (sub (Var "x") (Var "y")) [("x", Var "x"), ("y", Var "y")] `shouldBe` Ok (sub (Var "x") (Var "y"))
--     it "(-) x y  Γ{x: 1, y: 2}  ∴  -1" $ do
--       reduce (sub (Var "x") (Var "y")) [("x", Num 1), ("y", Num 2)] `shouldBe` Ok (Num (-1))

--   describe "☯︎ Multiplication" $ do
--     it "(*)  ∴  (*)" $ do
--       reduce Mul [] `shouldBe` Ok Mul
--     it "(*) x  Γ{x: x}  ∴  (*) x" $ do
--       reduce (App Mul (Var "x")) [("x", Var "x")] `shouldBe` Ok (App Mul (Var "x"))
--     it "(*) x y  Γ{x: x, y: y}  ∴  x * y" $ do
--       reduce (mul (Var "x") (Var "y")) [("x", Var "x"), ("y", Var "y")] `shouldBe` Ok (mul (Var "x") (Var "y"))
--     it "(*) x y  Γ{x: 1, y: 2}  ∴  2" $ do
--       reduce (mul (Var "x") (Var "y")) [("x", Num 1), ("y", Num 2)] `shouldBe` Ok (Num 2)

--   describe "☯︎ Equals" $ do
--     it "(==)  ∴  (==)" $ do
--       reduce Eq [] `shouldBe` Ok Eq
--     it "(==) x  Γ{x: x}  ∴  (==) x" $ do
--       reduce (App Eq (Var "x")) [("x", Var "x")] `shouldBe` Ok (App Eq (Var "x"))
--     it "(==) x y  Γ{x: x, y: y}  ∴  x == y" $ do
--       reduce (eq (Var "x") (Var "y")) [("x", Var "x"), ("y", Var "y")] `shouldBe` Ok (eq (Var "x") (Var "y"))
--     it "(==) x y  Γ{x: 1, y: 2}  ∴  λTrue False. False" $ do
--       reduce (eq (Var "x") (Var "y")) [("x", Num 1), ("y", Num 2)] `shouldBe` Ok (Lam "True" (Lam "False" (Var "False")))
--     it "(==) x y  Γ{x: 2, y: 2}  ∴  λTrue False. False" $ do
--       reduce (eq (Var "x") (Var "y")) [("x", Num 2), ("y", Num 2)] `shouldBe` Ok (Lam "True" (Lam "False" (Var "True")))
--     it "(==) x y  Γ{x: 3, y: 2}  ∴  λTrue False. False" $ do
--       reduce (eq (Var "x") (Var "y")) [("x", Num 3), ("y", Num 2)] `shouldBe` Ok (Lam "True" (Lam "False" (Var "False")))

--   describe "☯︎ Recursive definition" $ do
--     it "x@1  ∴  1" $ do
--       reduce (Rec "x" (Num 1)) [] `shouldBe` Ok (Num 1)
--     it "x@x  ∴  x@x" $ do
--       reduce (Rec "x" (Var "x")) [] `shouldBe` Ok (Rec "x" (Var "x"))
--     it "λy. x@x  ∴  @(λy. x)" $ do
--       reduce (Lam "y" (Rec "x" (Var "x"))) [] `shouldBe` Ok (Rec "x" (Lam "y" (Var "x")))
--     it "x@x y  Γ{y: y}  ∴  x@(x y)" $ do
--       reduce (App (Rec "x" (Var "x")) (Var "y")) [("y", Var "y")] `shouldBe` Ok (Rec "x" (App (Var "x") (Var "y")))
--     it "x y@y  Γ{x: x}  ∴  y@(x y)" $ do
--       reduce (App (Var "x") (Rec "y" (Var "y"))) [("x", Var "x")] `shouldBe` Ok (Rec "y" (App (Var "x") (Var "y")))

--   describe "☯︎ Factorial" $ do
--     it "f  Γ{f: factorial}  ∴  f@factorial" $ do
--       reduce (Var "f") [("f", factorial)] `shouldBe` Ok (Rec "f" factorial)
--     it "f 0  Γ{f: factorial}  ∴  1" $ do
--       reduce (App (Var "f") (Num 0)) [("f", factorial)] `shouldBe` Ok (Num 1)
--     it "f 1  Γ{f: factorial}  ∴  1" $ do
--       reduce (App (Var "f") (Num 1)) [("f", factorial)] `shouldBe` Ok (Num 1)
--     it "f 2  Γ{f: factorial}  ∴  2" $ do
--       reduce (App (Var "f") (Num 2)) [("f", factorial)] `shouldBe` Ok (Num 2)
--     it "f 5  Γ{f: factorial}  ∴  120" $ do
--       reduce (App (Var "f") (Num 5)) [("f", factorial)] `shouldBe` Ok (Num 120)

--   describe "☯︎ Ackermann" $ do
--     it "a  Γ{a: ackermann}  ∴  a@ackermann" $ do
--       reduce (Var "a") [("a", ackermann)] `shouldBe` Ok (Rec "a" ackermann)
--     it "a 0  Γ{a: ackermann}  ∴  λn. n + 1" $ do
--       reduce (App (Var "a") (Num 0)) [("a", ackermann)] `shouldBe` Ok (Lam "n" (add (Var "n") (Num 1)))
--     it "a 0 0  Γ{a: ackermann}  ∴  1" $ do
--       reduce (app (Var "a") [Num 0, Num 0]) [("a", ackermann)] `shouldBe` Ok (Num 1)
--     it "a 0 1  Γ{a: ackermann}  ∴  2" $ do
--       reduce (app (Var "a") [Num 0, Num 1]) [("a", ackermann)] `shouldBe` Ok (Num 2)
--     it "a 1 0  Γ{a: ackermann}  ∴  2" $ do
--       reduce (app (Var "a") [Num 1, Num 0]) [("a", ackermann)] `shouldBe` Ok (Num 2)
--     it "a 1 1  Γ{a: ackermann}  ∴  3" $ do
--       reduce (app (Var "a") [Num 1, Num 1]) [("a", ackermann)] `shouldBe` Ok (Num 3)
--     it "a 1 2  Γ{a: ackermann}  ∴  4" $ do
--       reduce (app (Var "a") [Num 1, Num 2]) [("a", ackermann)] `shouldBe` Ok (Num 4)
--     it "a 2 1  Γ{a: ackermann}  ∴  5" $ do
--       reduce (app (Var "a") [Num 2, Num 1]) [("a", ackermann)] `shouldBe` Ok (Num 5)
--     it "a 2 2  Γ{a: ackermann}  ∴  7" $ do
--       reduce (app (Var "a") [Num 2, Num 2]) [("a", ackermann)] `shouldBe` Ok (Num 7)
--     it "a 3 1  Γ{a: ackermann}  ∴  13" $ do
--       reduce (app (Var "a") [Num 3, Num 1]) [("a", ackermann)] `shouldBe` Ok (Num 13)
--     it "a 3 2  Γ{a: ackermann}  ∴  29" $ do
--       reduce (app (Var "a") [Num 3, Num 2]) [("a", ackermann)] `shouldBe` Ok (Num 29)
--     it "a 3 3  Γ{a: ackermann}  ∴  61" $ do
--       reduce (app (Var "a") [Num 3, Num 3]) [("a", ackermann)] `shouldBe` Ok (Num 61)
-- where
--   a = Var "a"
--   f = Var "f"
--   m = Var "m"
--   n = Var "n"
--   k0 = Num 0
--   k1 = Num 1

--   -- f 0 = 1
--   -- f n = n * f (n - 1)
--   factorial =
--     Lam "n" (app (eq n k0) [k1, mul n (App f (sub n k1))])

--   -- a 0 n = n + 1
--   -- a m 0 = a (m-1) 1
--   -- a m n = a (m-1) (a m (n-1))
--   ackermann =
--     Lam "m" (Lam "n" (app (eq m k0) [add n k1, app (eq n k0) [app a [sub m k1, k1], app a [sub m k1, app a [m, sub n k1]]]]))
