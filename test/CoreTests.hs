module CoreTests where

import Core
import Test.Hspec

coreTests :: SpecWith ()
coreTests = describe "--== Core ==--" $ do
  let (i0, i1, i2, i3) = (Int 0, Int 1, Int 2, Int 3)
  let (x, y, z) = (Var "x", Var "y", Var "z")
  -- describe "☯ show" $ do
  --   it "☯ basic expressions" $ do
  --     show Any `shouldBe` "_"
  --     show Tup `shouldBe` "()"
  --     show Add `shouldBe` "(+)"
  --     show Sub `shouldBe` "(-)"
  --     show Mul `shouldBe` "(*)"
  --     show IntT `shouldBe` "%Int"
  --     show i1 `shouldBe` "1"
  --     show x `shouldBe` "x"
  --     show (Let [] x) `shouldBe` "x"
  --     show (Let [("x", i1)] y) `shouldBe` "x = 1; y"
  --     show (Let [("x", i1), ("y", i2)] z) `shouldBe` "x = 1; y = 2; z"

  --   it "☯ operator precedence" $ do
  --     show (Or i1 (Or i2 i3)) `shouldBe` "1 | 2 | 3"
  --     show (Or (Or i1 i2) i3) `shouldBe` "(1 | 2) | 3"
  --     show (Or i1 (Ann i2 i3)) `shouldBe` "1 | 2 : 3"
  --     show (Or (Ann i1 i2) i3) `shouldBe` "1 : 2 | 3"
  --     show (Ann i1 (Ann i2 i3)) `shouldBe` "1 : 2 : 3"
  --     show (Ann (Ann i1 i2) i3) `shouldBe` "(1 : 2) : 3"
  --     show (Ann i1 (Lam i2 i3)) `shouldBe` "1 : 2 -> 3"
  --     show (Ann (Lam i1 i2) i3) `shouldBe` "1 -> 2 : 3"
  --     show (Lam i1 (Lam i2 i3)) `shouldBe` "1 -> 2 -> 3"
  --     show (Lam (Lam i1 i2) i3) `shouldBe` "(1 -> 2) -> 3"
  --     show (Lam i1 (add i2 i3)) `shouldBe` "1 -> 2 + 3"
  --     show (Lam i1 (sub i2 i3)) `shouldBe` "1 -> 2 - 3"
  --     show (Lam (add i1 i2) i3) `shouldBe` "1 + 2 -> 3"
  --     show (Lam (sub i1 i2) i3) `shouldBe` "1 - 2 -> 3"
  --     show (add (add i1 i2) i3) `shouldBe` "1 + 2 + 3"
  --     show (add i1 (add i2 i3)) `shouldBe` "1 + (2 + 3)"
  --     show (add i1 (sub i2 i3)) `shouldBe` "1 + (2 - 3)"
  --     show (add (sub i1 i2) i3) `shouldBe` "1 - 2 + 3"
  --     show (add i1 (mul i2 i3)) `shouldBe` "1 + 2 * 3"
  --     show (add (mul i1 i2) i3) `shouldBe` "1 * 2 + 3"
  --     show (sub (add i1 i2) i3) `shouldBe` "1 + 2 - 3"
  --     show (sub i1 (add i2 i3)) `shouldBe` "1 - (2 + 3)"
  --     show (sub i1 (sub i2 i3)) `shouldBe` "1 - (2 - 3)"
  --     show (sub (sub i1 i2) i3) `shouldBe` "1 - 2 - 3"
  --     show (sub i1 (mul i2 i3)) `shouldBe` "1 - 2 * 3"
  --     show (sub (mul i1 i2) i3) `shouldBe` "1 * 2 - 3"
  --     show (mul (mul i1 i2) i3) `shouldBe` "1 * 2 * 3"
  --     show (mul i1 (mul i2 i3)) `shouldBe` "1 * (2 * 3)"
  --     show (mul i1 (App i2 i3)) `shouldBe` "1 * 2 3"
  --     show (mul (App i1 i2) i3) `shouldBe` "1 2 * 3"
  --     show (App (App i1 i2) i3) `shouldBe` "1 2 3"
  --     show (App i1 (App i2 i3)) `shouldBe` "1 (2 3)"

  -- describe "☯ parse" $ do
  --   it "☯ terms" $ do
  --     parse "_" `shouldBe` Right Any
  --     parse "()" `shouldBe` Right Tup
  --     parse "( )" `shouldBe` Right Tup
  --     parse "(+)" `shouldBe` Right Add
  --     parse "( + )" `shouldBe` Right Add
  --     parse "(-)" `shouldBe` Right Sub
  --     parse "( - )" `shouldBe` Right Sub
  --     parse "(*)" `shouldBe` Right Mul
  --     parse "( * )" `shouldBe` Right Mul
  --     parse "%Int" `shouldBe` Right IntT
  --     parse "1" `shouldBe` Right i1
  --     parse "x" `shouldBe` Right x

  --   it "☯ unary operators" $ do
  --     -- parse "x = 1; y" `shouldBe` Right (Let [("x", i1)] y)
  --     -- parse "x = 1; y = 2; z" `shouldBe` Right (Let [("x", i1), ("y", i2)] z)
  --     parse "(_)" `shouldBe` Right Any
  --     parse "( _ )" `shouldBe` Right Any

  --   it "☯ binary operators" $ do
  --     parse "1|2" `shouldBe` Right (Or i1 i2)
  --     parse "1 | 2" `shouldBe` Right (Or i1 i2)
  --     parse "1:2" `shouldBe` Right (Ann i1 i2)
  --     parse "1 : 2" `shouldBe` Right (Ann i1 i2)
  --     parse "1->2" `shouldBe` Right (Lam i1 i2)
  --     parse "1 -> 2" `shouldBe` Right (Lam i1 i2)
  --     parse "1+2" `shouldBe` Right (add i1 i2)
  --     parse "1 + 2" `shouldBe` Right (add i1 i2)
  --     parse "1-2" `shouldBe` Right (sub i1 i2)
  --     parse "1 - 2" `shouldBe` Right (sub i1 i2)
  --     parse "1*2" `shouldBe` Right (mul i1 i2)
  --     parse "1 * 2" `shouldBe` Right (mul i1 i2)
  --     parse "1 2" `shouldBe` Right (App i1 i2)
  --     parse "1  2" `shouldBe` Right (App i1 i2)

  --   it "☯ operator precedence" $ do
  --     parse "1 | 2 | 3" `shouldBe` Right (Or i1 (Or i2 i3))
  --     parse "(1 | 2) | 3" `shouldBe` Right (Or (Or i1 i2) i3)
  --     parse "1 | 2 : 3" `shouldBe` Right (Or i1 (Ann i2 i3))
  --     parse "1 : 2 | 3" `shouldBe` Right (Or (Ann i1 i2) i3)
  --     parse "1 : 2 : 3" `shouldBe` Right (Ann i1 (Ann i2 i3))
  --     parse "(1 : 2) : 3" `shouldBe` Right (Ann (Ann i1 i2) i3)
  --     parse "1 : 2 -> 3" `shouldBe` Right (Ann i1 (Lam i2 i3))
  --     parse "1 -> 2 : 3" `shouldBe` Right (Ann (Lam i1 i2) i3)
  --     parse "1 -> 2 -> 3" `shouldBe` Right (Lam i1 (Lam i2 i3))
  --     parse "(1 -> 2) -> 3" `shouldBe` Right (Lam (Lam i1 i2) i3)
  --     parse "1 -> 2 + 3" `shouldBe` Right (Lam i1 (add i2 i3))
  --     parse "1 -> 2 - 3" `shouldBe` Right (Lam i1 (sub i2 i3))
  --     parse "1 + 2 -> 3" `shouldBe` Right (Lam (add i1 i2) i3)
  --     parse "1 - 2 -> 3" `shouldBe` Right (Lam (sub i1 i2) i3)
  --     parse "1 + 2 + 3" `shouldBe` Right (add (add i1 i2) i3)
  --     parse "1 + (2 + 3)" `shouldBe` Right (add i1 (add i2 i3))
  --     parse "1 + (2 - 3)" `shouldBe` Right (add i1 (sub i2 i3))
  --     parse "1 - 2 + 3" `shouldBe` Right (add (sub i1 i2) i3)
  --     parse "1 + 2 * 3" `shouldBe` Right (add i1 (mul i2 i3))
  --     parse "1 * 2 + 3" `shouldBe` Right (add (mul i1 i2) i3)
  --     parse "1 + 2 - 3" `shouldBe` Right (sub (add i1 i2) i3)
  --     parse "1 - (2 + 3)" `shouldBe` Right (sub i1 (add i2 i3))
  --     parse "1 - (2 - 3)" `shouldBe` Right (sub i1 (sub i2 i3))
  --     parse "1 - 2 - 3" `shouldBe` Right (sub (sub i1 i2) i3)
  --     parse "1 - 2 * 3" `shouldBe` Right (sub i1 (mul i2 i3))
  --     parse "1 * 2 - 3" `shouldBe` Right (sub (mul i1 i2) i3)
  --     parse "1 * 2 * 3" `shouldBe` Right (mul (mul i1 i2) i3)
  --     parse "1 * (2 * 3)" `shouldBe` Right (mul i1 (mul i2 i3))
  --     parse "1 * 2 3" `shouldBe` Right (mul i1 (App i2 i3))
  --     parse "1 2 * 3" `shouldBe` Right (mul (App i1 i2) i3)
  --     parse "1 2 3" `shouldBe` Right (App (App i1 i2) i3)
  --     parse "1 (2 3)" `shouldBe` Right (App i1 (App i2 i3))

  describe "☯ helper functions" $ do
    it "☯ let'" $ do
      let' [] Tup `shouldBe` Tup
      let' [("x", i1)] Tup `shouldBe` Let ("x", i1) Tup
      let' [("x", i1), ("y", i2), ("z", i3)] Tup `shouldBe` Let ("x", i1) (Let ("y", i2) (Let ("z", i3) Tup))
    it "☯ app" $ do
      app Tup [] `shouldBe` Tup
      app Tup [x] `shouldBe` App Tup x
      app Tup [x, y, z] `shouldBe` App (App (App Tup x) y) z
    it "☯ add" $ do
      add x y `shouldBe` App (App Add x) y
    it "☯ sub" $ do
      sub x y `shouldBe` App (App Sub x) y
    it "☯ mul" $ do
      mul x y `shouldBe` App (App Mul x) y

  describe "☯ reduction rules" $ do
    it "☯ reduce" $ do
      -- Default cases
      reduce Any [] `shouldBe` Right (Any, [])
      reduce Tup [] `shouldBe` Right (Tup, [])
      reduce Add [] `shouldBe` Right (Add, [])
      reduce Sub [] `shouldBe` Right (Sub, [])
      reduce Mul [] `shouldBe` Right (Mul, [])
      reduce IntT [] `shouldBe` Right (IntT, [])
      reduce i1 [] `shouldBe` Right (i1, [])
      reduce (Or x y) [] `shouldBe` Right (Or x y, [])
      reduce (Ann x y) [] `shouldBe` Right (Ann x y, [])
      reduce (Lam x y) [] `shouldBe` Right (Lam x y, [])
      reduce (App Tup x) [] `shouldBe` Right (App Tup x, [])
      -- Variable lookup
      reduce x [] `shouldBe` Left (UndefinedVariable "x")
      reduce x [("x", x)] `shouldBe` Right (x, [("x", x)])
      reduce x [("x", i1), ("y", i2)] `shouldBe` Right (i1, [("x", i1), ("y", i2)])
      reduce y [("x", i1), ("y", i2)] `shouldBe` Right (i2, [("x", i1), ("y", i2)])
      -- Arithmetic reduction
      reduce (add x x) [("x", x)] `shouldBe` Right (add x x, [("x", x)])
      reduce (add x x) [("x", i1)] `shouldBe` Right (i2, [("x", i1)])
      reduce (sub x x) [("x", i1)] `shouldBe` Right (i0, [("x", i1)])
      reduce (mul x x) [("x", i1)] `shouldBe` Right (i1, [("x", i1)])
      -- Variable reduction
      reduce x [("x", add i1 i1)] `shouldBe` Right (i2, [("x", i2)])
      -- Let binding
      reduce (Let ("x", add y y) x) [("y", add i0 i1)] `shouldBe` Right (i2, [("x", i2), ("y", i1)])
      -- Pattern matching
      reduce (App (Lam i1 i2) (add i1 i0)) [] `shouldBe` Right (i2, [])
      reduce (App (Lam i1 i2) (add i1 i2)) [] `shouldBe` Left (PatternMismatch i1 i3)
      -- Pattern matching alternatives
      reduce (App (Or (Lam i1 i2) (Lam i2 i3)) (add i1 i0)) [] `shouldBe` Right (i2, [])
      reduce (App (Or (Lam i1 i2) (Lam i2 i3)) (add i1 i1)) [] `shouldBe` Right (i3, [])
      reduce (App (Or (Lam i1 i2) (Lam i2 i3)) (add i0 i0)) [] `shouldBe` Left (PatternMismatch i2 i0)
      -- Indirect reduction
      reduce (App x i1) [("x", App Add i1)] `shouldBe` Right (i2, [("x", App Add i1)])
      reduce (App x i1) [("x", App Sub i1)] `shouldBe` Right (i0, [("x", App Sub i1)])
      reduce (App x i1) [("x", App Mul i1)] `shouldBe` Right (i1, [("x", App Mul i1)])
      reduce (App x i1) [("x", Lam i1 i2)] `shouldBe` Right (i2, [("x", Lam i1 i2)])
      reduce (App x i1) [("x", Or (Lam i1 i2) (Lam i2 i3))] `shouldBe` Right (i2, [("x", Or (Lam i1 i2) (Lam i2 i3))])

    it "☯ match" $ do
      match Any Tup [] `shouldBe` Right []
      match Tup Any [] `shouldBe` Left (PatternMismatch Tup Any)
      match Tup Tup [] `shouldBe` Right []
      match Add Add [] `shouldBe` Right []
      match Sub Sub [] `shouldBe` Right []
      match Mul Mul [] `shouldBe` Right []
      match IntT IntT [] `shouldBe` Right []
      match i1 i1 [] `shouldBe` Right []
      match i1 i2 [] `shouldBe` Left (PatternMismatch i1 i2)
      match i1 (add i1 i1) [] `shouldBe` Left (PatternMismatch i1 i2)
      match i2 (add i1 i1) [] `shouldBe` Right []
      match x (add i1 i1) [] `shouldBe` Left (PatternMismatch x i2)
      match x (add i1 i1) [("x", x), ("y", y)] `shouldBe` Right [("x", add i1 i1), ("y", y)]
      match y (add i1 i1) [("x", x), ("y", y)] `shouldBe` Right [("x", x), ("y", add i1 i1)]
      match (Or x i0) (add i1 i1) [("x", x)] `shouldBe` Right [("x", add i1 i1)]
      match (Or i0 x) (add i1 i1) [("x", x)] `shouldBe` Right [("x", i2)]
      match (Or i0 i1) (add i1 i1) [] `shouldBe` Left (PatternMismatch i1 i2)
      match i1 (Or i1 i2) [] `shouldBe` Right []
      match i2 (Or i1 i2) [] `shouldBe` Right []
      match i3 (Or i1 i2) [] `shouldBe` Left (PatternMismatch i3 i2)
      match (Ann x y) (Ann i1 IntT) [("x", x), ("y", y)] `shouldBe` Right [("x", i1), ("y", IntT)]
      match (Ann x y) i1 [("x", x), ("y", y)] `shouldBe` Right [("x", i1), ("y", IntT)]
      match (Lam x y) (Lam i1 i2) [("x", x), ("y", y)] `shouldBe` Right [("x", i1), ("y", i2)]
      -- TODO: outputs are correct, but simplify examples
      -- match (Lam x y) z [("x", x), ("y", y), ("z", Lam i1 i2)] `shouldBe` Right [("x", i1), ("y", i2), ("z", Lam i1 i2)]
      -- match (App x y) z [("x", x), ("y", y), ("z", App Tup i1)] `shouldBe` Right [("x", Tup), ("y", i1), ("z", App Tup i1)]
      -- match (App x y) z [("x", x), ("y", y), ("z", App Tup i1)] `shouldBe` Right [("x", Tup), ("y", i1), ("z", App Tup i1)]
      True `shouldBe` True

      -- TODO: fix this!
      -- reduce (App (Let ("x", x) $ Lam x x) i2) [("x", x)] `shouldBe` Right (i0, [])
      -- match (Lam i1 x) (App (Let ("y", y) $ Lam i2 $ Lam i1 i0) i2) [("x", x)] `shouldBe` Right []
      -- @x. 1 -> x `match` (@y. y -> 1 -> y) 2 []
      -- @x. 1 -> x `match` (1 -> y) [y=2]
      -- match (Lam i1 x) (App (For "y" (Lam y (Lam i1 y))) i2) [("x", Any)] `shouldBe` Right [("x", i2)]
      -- match' "1 -> x" "(@y. y -> 1 -> y) 2" [("x", Any), ("y", Any)] `shouldBe` Right [("x", Var "y"), ("y", i2)]
      -- match' "x 1" "() 1" [("x", Any)] `shouldBe` Right [("x", Tup)]
      -- match' "() x" "() 1" [("x", Any)] `shouldBe` Right [("x", i1)]
      -- match' "() x" "(y -> () y) 2" [("x", Any), ("y", Any)] `shouldBe` Right [("x", Var "y"), ("y", i2)]

      -- it "☯ eval" $ do
      --   eval Any [] `shouldBe` Any
      --   eval Tup [] `shouldBe` Tup
      --   eval Add [] `shouldBe` Add
      --   eval Sub [] `shouldBe` Sub
      --   eval Mul [] `shouldBe` Mul
      --   eval IntT [] `shouldBe` IntT
      --   eval i1 [] `shouldBe` i1
      --   eval x [] `shouldBe` x
      --   eval x [("x", i1)] `shouldBe` i1
      --   -- eval (For "x" y) [("y", i1)] `shouldBe` i1
      --   eval (Or x y) [("x", i1), ("y", i2)] `shouldBe` Or i1 i2
      --   eval (Ann x IntT) [("x", i1)] `shouldBe` i1
      --   eval (Lam x y) [("x", Tup), ("y", i1)] `shouldBe` Lam Tup i1
      --   eval (App x y) [("x", Tup), ("y", i1)] `shouldBe` App Tup i1
      --   eval (App (Lam i1 i2) i1) [] `shouldBe` i2
      --   eval (App (Or (Lam i1 i2) (Lam i2 i3)) i1) [] `shouldBe` i2
      --   eval (App (Or (Lam i1 i2) (Lam i2 i3)) i2) [] `shouldBe` i3
      --   eval (add x x) [("x", i1)] `shouldBe` i2
      --   eval (sub x x) [("x", i1)] `shouldBe` i0
      --   eval (mul x x) [("x", i1)] `shouldBe` i1

      -- it "☯ typeOf" $ do
      -- Any
      -- Tup
      -- Add
      -- Sub
      -- Mul
      -- IntT
      -- Int Int
      -- Var Int
      -- For String Expr
      -- Or Expr Expr
      -- Ann Expr Typ
      -- Lam Pattern Expr
      -- App Expr Expr

      True `shouldBe` True