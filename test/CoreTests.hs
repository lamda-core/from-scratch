module CoreTests where

import Core
import Test.Hspec

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

  it "☯ match" $ do
    let ctx = defineConstructors [("A", 0), ("B", 1)] []
    match [] ctx `shouldBe` Err
    match [([], Int 1), ([], Int 2)] ctx `shouldBe` Int 1
    match [([(PAny, "x")], Var "x")] ctx `shouldBe` Lam "%0" (let' ("x", Var "%0") (Var "x"))
    match [([(PAny, "x")], Var "x")] ctx `shouldBe` Lam "%0" (let' ("x", Var "%0") (Var "x"))
    match [([(PCtr "Unknown" [], "x")], Var "x")] ctx `shouldBe` Lam "%0" Err
    match [([(PCtr "A" [], "x")], Var "x")] ctx `shouldBe` Lam "%0" (app (Var "%0") [let' ("x", Var "%0") (Var "x"), Err])
    match [([(PCtr "B" [(PAny, "a")], "x")], Var "x")] ctx `shouldBe` Lam "%0" (app (Var "%0") [Err, Lam "%1" (let' ("a", Var "%1") (let' ("x", Var "%0") (Var "x")))])

  it "☯ nameIndex" $ do
    nameIndex "" "" `shouldBe` Nothing
    nameIndex "" "x" `shouldBe` Nothing
    nameIndex "" "42" `shouldBe` Just 42
    nameIndex "x" "x42" `shouldBe` Just 42
    nameIndex "x" "y42" `shouldBe` Nothing

  it "☯ lastNameIndex" $ do
    lastNameIndex "x" [] `shouldBe` Nothing
    lastNameIndex "x" ["x"] `shouldBe` Just 0
    lastNameIndex "x" ["x1"] `shouldBe` Just 1
    lastNameIndex "x" ["x", "x1"] `shouldBe` Just 1
    lastNameIndex "x" ["x1", "x"] `shouldBe` Just 1
    lastNameIndex "x" ["x1", "x2"] `shouldBe` Just 2
    lastNameIndex "x" ["x2", "x1"] `shouldBe` Just 2

  it "☯ freeVariables" $ do
    freeVariables (Var "x") `shouldBe` ["x"]
    freeVariables (Int 1) `shouldBe` []
    freeVariables (App (Var "x") (Var "x")) `shouldBe` ["x"]
    freeVariables (App (Var "x") (Var "y")) `shouldBe` ["x", "y"]
    freeVariables (Lam "x" (Var "x")) `shouldBe` []
    freeVariables (Lam "x" (Var "y")) `shouldBe` ["y"]
    freeVariables (Op2 Add) `shouldBe` []

  it "☯ newName" $ do
    newName [] "x" `shouldBe` "x0"
    newName ["x"] "x" `shouldBe` "x1"
    newName ["x", "x1"] "x" `shouldBe` "x2"
