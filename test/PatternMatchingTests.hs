module PatternMatchingTests where

import PatternMatching
import Test.Hspec

patternMatchingTests :: SpecWith ()
patternMatchingTests = describe "--== Pattern matching ==--" $ do
  let (x, y, z) = (Var "x", Var "y", Var "z")
  let (a, b, c) = (Ctr "A", Ctr "B", Ctr "C")

  it "☯ occurs" $ do
    occurs "x" Err `shouldBe` False
    occurs "x" (Var "x") `shouldBe` True
    occurs "x" (Var "y") `shouldBe` False
    occurs "x" (Ctr "x") `shouldBe` False
    occurs "x" (Let ("x", a) x) `shouldBe` False
    occurs "x" (Let ("y", a) y) `shouldBe` False
    occurs "x" (Let ("y", a) x) `shouldBe` True
    occurs "x" (Lam x x) `shouldBe` False
    occurs "x" (Lam y y) `shouldBe` False
    occurs "x" (Lam y x) `shouldBe` True
    occurs "x" (Or y y) `shouldBe` False
    occurs "x" (Or x y) `shouldBe` True
    occurs "x" (Or y x) `shouldBe` True
    occurs "x" (App y y) `shouldBe` False
    occurs "x" (App x y) `shouldBe` True
    occurs "x" (App y x) `shouldBe` True

  describe "☯ reduce" $ do
    it "☯ base cases" $ do
      reduce Err `shouldBe` Err
      reduce x `shouldBe` x
      reduce a `shouldBe` a
    it "☯ let bindings" $ do
      reduce (Let ("x", a) Err) `shouldBe` Err
      reduce (Let ("x", a) x) `shouldBe` a
      reduce (Let ("x", a) y) `shouldBe` y
      reduce (Let ("y", a) $ Let ("x", y) x) `shouldBe` a
      reduce (Let ("x", a) $ Lam x y) `shouldBe` Lam x y
      reduce (Let ("y", a) $ Lam x y) `shouldBe` Lam x (Let ("y", a) y)
      reduce (Let ("x", a) $ Or x y) `shouldBe` Or (Let ("x", a) x) (Let ("x", a) y)
      reduce (Let ("x", a) $ App x y) `shouldBe` App a (Let ("x", a) y)
    it "☯ alternation" $ do
      reduce (Or x y) `shouldBe` x
    it "☯ pattern matching" $ do
      reduce (Let ("x", Err) $ App (Lam Err y) x) `shouldBe` y
      reduce (Let ("x", a) $ App (Lam Err y) x) `shouldBe` Err
      reduce (Let ("x", b) $ App (Lam y y) x) `shouldBe` b
      reduce (Let ("x", a) $ App (Lam a b) x) `shouldBe` b
      reduce (Let ("x", b) $ App (Lam a b) x) `shouldBe` Err
      reduce (Let ("x", a) $ App (Lam (Let ("y", a) y) b) x) `shouldBe` b
      -- TODO: pattern Lam
      reduce (Let ("x", a) $ App (Lam (Or a b) c) x) `shouldBe` c
      reduce (Let ("x", b) $ App (Lam (Or a b) c) x) `shouldBe` c
      reduce (Let ("x", c) $ App (Lam (Or a b) c) x) `shouldBe` Err
      reduce (Let ("x", App a b) $ App (Lam (App x y) x) x) `shouldBe` a
      reduce (Let ("x", App a b) $ App (Lam (App x y) y) x) `shouldBe` b
      reduce (Let ("x", a) $ App (Lam (App x y) y) x) `shouldBe` Err
