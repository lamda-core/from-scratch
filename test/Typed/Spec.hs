import Test.Hspec
import Typed

main :: IO ()
main = hspec $ do
  describe "--== Unify ==--" $ do
    it "❌ Num = Type  ∴  Type mismatch: Num ≠ Type" $ do
      unify NumT TypT [] `shouldBe` Err (TypeMismatch NumT TypT)
    it "✅ Num = Num  ∴  Num" $ do
      unify NumT NumT [] `shouldBe` Ok (NumT, [])
    it "❌ x = Num  ∴  Undefined variable: x" $ do
      unify (VarT "x") NumT [] `shouldBe` Err (UndefinedVar "x")
    it "❌ Num = x ∴  Undefined variable: x" $ do
      unify NumT (VarT "x") [] `shouldBe` Err (UndefinedVar "x")
    it "✅ x = Num  Γ{x: Num}  ∴  Num  Γ{x: Num}" $ do
      unify (VarT "x") NumT [("x", NumT)] `shouldBe` Ok (NumT, [("x", NumT)])
    it "✅ Num = x Γ{x: Num}  ∴  Num  Γ{x: Num}" $ do
      unify NumT (VarT "x") [("x", NumT)] `shouldBe` Ok (NumT, [("x", NumT)])
    it "✅ ∀x = Num  ∴  Num  Γ{x: Num}" $ do
      unify (AnyT "x") NumT [] `shouldBe` Ok (NumT, [("x", NumT)])
    it "✅ Num = ∀x  ∴  Num  Γ{x: Num}" $ do
      unify NumT (AnyT "x") [] `shouldBe` Ok (NumT, [("x", NumT)])
    it "❌ ∀x -> Num = Num -> Type  ∴  Type mismatch: Num ≠ Type" $ do
      unify (FunT (AnyT "x") NumT) (FunT NumT TypT) [] `shouldBe` Err (TypeMismatch NumT TypT)
    it "✅ ∀x -> Num = Type -> Num  ∴  Type -> Num  Γ{x: Type}" $ do
      unify (FunT (AnyT "x") NumT) (FunT TypT NumT) [] `shouldBe` Ok (FunT TypT NumT, [("x", TypT)])
    it "✅ ∀x -> x = Num -> Num  ∴  Num -> Num  Γ{x: Num}" $ do
      unify (FunT (AnyT "x") (VarT "x")) (FunT NumT NumT) [] `shouldBe` Ok (FunT NumT NumT, [("x", NumT)])
    it "❌ ∀x -> x = Num -> Type  ∴  Type mismatch: Num ≠ Type" $ do
      unify (FunT (AnyT "x") (VarT "x")) (FunT NumT NumT) [] `shouldBe` Ok (FunT NumT NumT, [("x", NumT)])

  describe "--== Types ==--" $ do
    describe "☯︎ Kind type" $ do
      it "✅ Type  ∴  Type : Type" $ do
        reduceT TypT [] `shouldBe` Ok (TypT, TypT)

    describe "☯︎ Number type" $ do
      it "✅ Num  ∴  Num : Type" $ do
        reduceT NumT [] `shouldBe` Ok (NumT, TypT)

    describe "☯︎ Any type" $ do
      it "✅ ∀x  ∴  ∀x" $ do
        reduceT (AnyT "x") [] `shouldBe` Ok (AnyT "x", TypT)

    describe "☯︎ Variable type" $ do
      it "❌ x  ∴  Undefined variable: x" $ do
        reduceT (VarT "x") [] `shouldBe` Err (UndefinedVar "x")
      it "✅ x  Γ{x: x}  ∴  x : x" $ do
        reduceT (VarT "x") [("x", VarT "x")] `shouldBe` Ok (VarT "x", VarT "x")
      it "✅ x  Γ{y: Num, x: y}  ∴  Num : Type" $ do
        reduceT (VarT "x") [("y", NumT), ("x", VarT "y")] `shouldBe` Ok (NumT, TypT)

    describe "☯︎ Function type" $ do
      it "❌ x -> Num  ∴  Undefined variable: x" $ do
        reduceT (FunT (VarT "x") NumT) [] `shouldBe` Err (UndefinedVar "x")
      it "❌ Num -> x  ∴  Undefined variable: x" $ do
        reduceT (FunT NumT (VarT "x")) [] `shouldBe` Err (UndefinedVar "x")
      it "✅ Num -> Type  ∴  Num -> Type : Type -> Type" $ do
        -- TODO: maybe this should be  ∴  Num -> Type : Type
        reduceT (FunT NumT TypT) [] `shouldBe` Ok (FunT NumT TypT, FunT TypT TypT)

  describe "--== Expressions ==--" $ do
    describe "☯︎ Number" $ do
      it "✅ 1  ∴  1 : Num" $ do
        reduce (Num 1) [] [] `shouldBe` Ok (Num 1, NumT)

    describe "☯︎ Variable" $ do
      it "✅ x  Γ{x: x}  ∴  x : ∀x" $ do
        reduce (Var "x") [("x", Var "x")] [] `shouldBe` Ok (Var "x", AnyT "x")
      it "✅ x  Γ{x: x : Num}  ∴  x : Num" $ do
        reduce (Var "x") [("x", Ann (Var "x") NumT)] [] `shouldBe` Ok (Var "x", NumT)
      it "✅ x  Γ{y: 1, x: y}  ∴  1 : Num" $ do
        reduce (Var "x") [("y", Num 1), ("x", Var "y")] [] `shouldBe` Ok (Num 1, NumT)

    describe "☯︎ λ Lamda abstraction" $ do
      it "✅ λx. 1  ∴  λx. 1 : ∀x -> Num" $ do
        reduce (Lam "x" (Num 1)) [] [] `shouldBe` Ok (Lam "x" (Num 1), FunT (AnyT "x") NumT)
      it "✅ λx. x  ∴  λx. x : ∀x -> x" $ do
        reduce (Lam "x" (Var "x")) [] [] `shouldBe` Ok (Lam "x" (Var "x"), FunT (AnyT "x") (VarT "x"))

    describe "☯︎ Application" $ do
      it "❌ 1 2  ∴  Not a function: 1 : Num" $ do
        reduce (App (Num 1) (Num 2)) [] [] `shouldBe` Err (NotAFunction (Num 1) NumT, [App (Num 1) (Num 2)])
      it "❌ f x  ∴  Undefined variable: f" $ do
        reduce (App (Var "f") (Var "x")) [] [] `shouldBe` Err (UndefinedVar "f", [App (Var "f") (Var "x"), Var "f"])
      it "❌ f x  Γ{f: f}  ∴  Undefined variable: x" $ do
        reduce (App (Var "f") (Var "x")) [("f", Var "f")] [] `shouldBe` Err (UndefinedVar "x", [App (Var "f") (Var "x"), Var "x"])
      it "❌ f 1  Γ{f: f : Num}  ∴  Not a function: f : Num" $ do
        reduce (App (Var "f") (Num 1)) [("f", Ann (Var "f") NumT)] [] `shouldBe` Err (NotAFunction (Var "f") NumT, [App (Var "f") (Num 1)])
      it "❌ f 1  Γ{f: f : Type -> Num}  ∴  Type mismatch: Num ≠ Type" $ do
        reduce (App (Var "f") (Num 1)) [("f", Ann (Var "f") (FunT TypT NumT))] [] `shouldBe` Err (TypeMismatch TypT NumT, [App (Var "f") (Num 1)])
      it "✅ f 1  Γ{f: f : Num -> Type}  ∴  f 1 : Type" $ do
        reduce (App (Var "f") (Num 1)) [("f", Ann (Var "f") (FunT NumT TypT))] [] `shouldBe` Ok (App (Var "f") (Num 1), TypT)
      it "✅ (λx. x) 1  ∴  1 : Num" $ do
        reduce (App (Ann (Lam "x" (Var "x")) (FunT NumT NumT)) (Num 1)) [] [] `shouldBe` Ok (Num 1, NumT)
      it "✅ ((λx. x) (λy. y)) 1  ∴  1 : Num" $ do
        reduce (App (App (Lam "x" (Var "x")) (Lam "y" (Var "y"))) (Num 1)) [] [] `shouldBe` Ok (Num 1, NumT)

    describe "☯︎ Recursive definition" $ do
      it "x@1  ∴  1 : Num" $ do
        reduce (Rec "x" (Num 1)) [] [] `shouldBe` Ok (Num 1, NumT)
      it "x@x  ∴  x@x : ∀x" $ do
        reduce (Rec "x" (Var "x")) [] [] `shouldBe` Ok (Rec "x" (Var "x"), AnyT "x")
      it "λy. x@x  ∴  @(λy. x)" $ do
        reduce (Lam "y" (Rec "x" (Var "x"))) [] [] `shouldBe` Ok (Rec "x" (Lam "y" (Var "x")), FunT (AnyT "y") (AnyT "x"))
    --   it "x@x y  Γ{y: y}  ∴  x@(x y)" $ do
    --     reduce (App (Rec "x" (Var "x")) (Var "y")) [("y", Var "y")] [] `shouldBe` Ok (Rec "x" (App (Var "x") (Var "y")), TypT)
    --   it "x y@y  Γ{x: x}  ∴  y@(x y)" $ do
    --     reduce (App (Var "x") (Rec "y" (Var "y"))) [("x", Var "x")] []`shouldBe` Ok (Rec "y" (App (Var "x") (Var "y")), TypT)

    describe "☯︎ Type annotation" $ do
      it "❌ x : y  ∴  Undefined variable: x" $ do
        reduce (Ann (Var "x") (VarT "y")) [] [] `shouldBe` Err (UndefinedVar "x", [Ann (Var "x") (VarT "y"), Var "x"])
      it "❌ 1 : x  ∴  Undefined variable: x" $ do
        reduce (Ann (Num 1) (VarT "x")) [] [] `shouldBe` Err (UndefinedVar "x", [Ann (Num 1) (VarT "x")])
      it "❌ 1 : Type  ∴  Type mismatch: Num ≠ Type" $ do
        reduce (Ann (Num 1) TypT) [] [] `shouldBe` Err (TypeMismatch NumT TypT, [Ann (Num 1) TypT])
      it "✅ 1 : Num  ∴  1 : Num" $ do
        reduce (Ann (Num 1) NumT) [] [] `shouldBe` Ok (Num 1, NumT)

    describe "☯︎ Addition" $ do
      it "(+)  ∴  (+) : Num -> Num -> Num" $ do
        reduce Add [] [] `shouldBe` Ok (Add, FunT NumT (FunT NumT NumT))

-- it "(+) x  Γ{x: x}  ∴  (+) x" $ do
--   reduce (App Add (Var "x")) [("x", Var "x")] `shouldBe` Ok (App Add (Var "x"))
-- it "(+) x y  Γ{x: x, y: y}  ∴  x + y" $ do
--   reduce (add (Var "x") (Var "y")) [("x", Var "x"), ("y", Var "y")] `shouldBe` Ok (add (Var "x") (Var "y"))
-- it "(+) x y  Γ{x: 1, y: 2}  ∴  3" $ do
--   reduce (add (Var "x") (Var "y")) [("x", Num 1), ("y", Num 2)] `shouldBe` Ok (Num 3)

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
