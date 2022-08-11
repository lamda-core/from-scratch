module ReducerTests where

import Core
import Test.Hspec

reducerTests :: String -> (Term -> Term) -> SpecWith ()
reducerTests name eval = describe ("--== ☯ " ++ name ++ " ☯ ==--") $ do
  it "☯ Constant expression" $ do
    -- Normal form
    eval Err `shouldBe` Err
    eval (Var "x") `shouldBe` Var "x"
    eval (Int 1) `shouldBe` Int 1
    eval (App (Var "x") (Var "y")) `shouldBe` App (Var "x") (Var "y")
    eval (Lam "x" (Var "y")) `shouldBe` Lam "x" (Var "y")
    eval (Call "f") `shouldBe` Call "f"

  it "☯ Lambda application" $ do
    -- Beta / β-reduction
    let (x, y, z) = (Var "x", Var "y", Var "z")
    eval (App (Lam "x" y) z) `shouldBe` y
    eval (App (Lam "x" x) z) `shouldBe` z
    eval (App (Lam "x" (App x x)) z) `shouldBe` App z z
    eval (App (Lam "x" (Lam "x" x)) z) `shouldBe` Lam "x" x
    eval (App (Lam "x" (Lam "y" x)) z) `shouldBe` Lam "y" z

  it "☯ Built-in functions" $ do
    -- Delta / δ-reduction
    let (i1, i2) = (int 1, int 2)
    eval (add i1 i1 empty) `shouldBe` Int 2
    eval (sub i1 i1 empty) `shouldBe` Int 0
    eval (mul i1 i1 empty) `shouldBe` Int 1
    eval (eq i1 i1 empty) `shouldBe` Lam "T" (Lam "F" (Var "T"))
    eval (eq i1 i2 empty) `shouldBe` Lam "T" (Lam "F" (Var "F"))
    eval (add (add i1 i1) (add i1 i1) empty) `shouldBe` Int 4
    eval (add (add i1 i1) (var "y") empty) `shouldBe` add (int 2) (var "y") empty
    eval (add (var "x") (add i1 i1) empty) `shouldBe` add (var "x") (int 2) empty
    eval (add (var "x") (var "y") empty) `shouldBe` add (var "x") (var "y") empty

  it "☯ Lambda reduction" $ do
    eval (Lam "x" (add (int 1) (int 1) empty)) `shouldBe` Lam "x" (Int 2)

  it "☯ Error reduction" $ do
    eval (App Err (Int 1)) `shouldBe` Err

  it "☯ Fixed point recursion" $ do
    eval (App Fix (Var "f")) `shouldBe` App (Var "f") (App Fix (Var "f"))
