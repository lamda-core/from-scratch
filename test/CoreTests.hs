module CoreTests where

import Core
import Test.Hspec

-- ctx :: Context
-- ctx = defineType "List" [("Nil", []), ("Cons", ["x", "xs"])] empty

coreTests :: SpecWith ()
coreTests = describe "--== Core language ==--" $ do
  it "☯ app" $ do
    app (Var "x") [] `shouldBe` Var "x"
    app (Var "x") [Var "y"] `shouldBe` App (Var "x") (Var "y")
    app (Var "x") [Var "y", Var "z"] `shouldBe` App (App (Var "x") (Var "y")) (Var "z")

  it "☯ lam" $ do
    lam [] (Int 1) `shouldBe` Int 1
    lam ["x"] (Int 1) `shouldBe` Lam "x" (Int 1)
    lam ["x", "y"] (Int 1) `shouldBe` Lam "x" (Lam "y" (Int 1))

  it "☯ built-in operators" $ do
    add (Var "x") (Var "y") `shouldBe` app (Op2 Add) [Var "x", Var "y"]
    sub (Var "x") (Var "y") `shouldBe` app (Op2 Sub) [Var "x", Var "y"]
    mul (Var "x") (Var "y") `shouldBe` app (Op2 Mul) [Var "x", Var "y"]
    eq (Var "x") (Var "y") `shouldBe` app (Op2 Eq) [Var "x", Var "y"]

  it "☯ if" $ do
    if' (Var "x") (Var "y") (Var "z") `shouldBe` app (Var "x") [Var "y", Var "z"]

  it "☯ nameIndex" $ do
    nameIndex "" "" `shouldBe` Nothing
    nameIndex "" "x" `shouldBe` Nothing
    nameIndex "" "42" `shouldBe` Just 42
    nameIndex "x" "x42" `shouldBe` Just 42
    nameIndex "x" "y42" `shouldBe` Nothing

  it "☯ findLastNameIndex" $ do
    findLastNameIndex "x" [] `shouldBe` Nothing
    findLastNameIndex "x" ["x"] `shouldBe` Just 0
    findLastNameIndex "x" ["x1"] `shouldBe` Just 1
    findLastNameIndex "x" ["x", "x1"] `shouldBe` Just 1
    findLastNameIndex "x" ["x1", "x"] `shouldBe` Just 1
    findLastNameIndex "x" ["x1", "x2"] `shouldBe` Just 2
    findLastNameIndex "x" ["x2", "x1"] `shouldBe` Just 2

  it "☯ freeVariables" $ do
    freeVariables (Var "x") `shouldBe` ["x"]
    freeVariables (Int 1) `shouldBe` []
    freeVariables (App (Var "x") (Var "x")) `shouldBe` ["x"]
    freeVariables (App (Var "x") (Var "y")) `shouldBe` ["x", "y"]
    freeVariables (Lam "x" (Var "x")) `shouldBe` []
    freeVariables (Lam "x" (Var "y")) `shouldBe` ["y"]
    freeVariables (Op2 Add) `shouldBe` []

  it "☯ newName" $ do
    newName [] "x" `shouldBe` "x"
    newName ["x"] "x" `shouldBe` "x1"
    newName ["x", "x1"] "x" `shouldBe` "x2"

  it "☯ newNames" $ do
    newNames [] [] `shouldBe` []
    newNames [] ["x"] `shouldBe` ["x"]
    newNames ["x"] ["x"] `shouldBe` ["x1"]
    newNames ["x"] ["x", "x"] `shouldBe` ["x1", "x2"]
    newNames ["x3"] ["x", "x"] `shouldBe` ["x4", "x5"]