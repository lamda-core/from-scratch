module CoreTests where

import Core
import Test.Hspec

coreTests :: SpecWith ()
coreTests = describe "--== Core language ==--" $ do
  it "☯ app" $ do
    app (var "x") [] empty `shouldBe` Var "x"
    app (var "x") [var "y"] empty `shouldBe` App (Var "x") (Var "y")
    app (var "x") [var "y", var "z"] empty `shouldBe` App (App (Var "x") (Var "y")) (Var "z")

  it "☯ lam" $ do
    lam [] (var "x") empty `shouldBe` Var "x"
    lam ["x"] (var "y") empty `shouldBe` Lam "x" (Var "y")
    lam ["x", "y"] (var "z") empty `shouldBe` Lam "x" (Lam "y" (Var "z"))

  it "☯ built-in operators" $ do
    add (var "x") (var "y") empty `shouldBe` App (App (Op2 Add) (Var "x")) (Var "y")
    sub (var "x") (var "y") empty `shouldBe` App (App (Op2 Sub) (Var "x")) (Var "y")
    mul (var "x") (var "y") empty `shouldBe` App (App (Op2 Mul) (Var "x")) (Var "y")
    eq (var "x") (var "y") empty `shouldBe` App (App (Op2 Eq) (Var "x")) (Var "y")

  it "☯ if" $ do
    if' (var "x") (var "y") (var "z") empty `shouldBe` App (App (Var "x") (Var "y")) (Var "z")

  it "☯ let'" $ do
    let' [] err empty `shouldBe` Err
    let' [("x", int 1)] (var "y") empty `shouldBe` Var "y"
    let' [("x", int 1)] (var "x") empty `shouldBe` App (Lam "x" (Var "x")) (Int 1)
    let' [("x", var "x")] (var "x") empty `shouldBe` App (Lam "x" (Var "x")) (Var "x")
    let' [("x", var "y"), ("y", int 1)] (var "x") empty `shouldBe` App (Lam "x" (Var "x")) (App (Lam "y" (Var "y")) (Int 1))
    let' [("x", int 1), ("y", var "x")] (var "y") empty `shouldBe` App (Lam "y" (Var "y")) (App (Lam "x" (Var "x")) (Int 1))

  it "☯ match" $ do
    let ctx = defineType "T" [] [("A", 0), ("B", 1)] empty
    match [] ctx `shouldBe` Err
    match [([], int 1), ([], int 2)] ctx `shouldBe` Int 1
    match [([(PAny, "x")], var "x")] ctx `shouldBe` Lam "%0" (App (Lam "x" (Var "x")) (Var "%0"))
    match [([(PInt 1, "x")], var "x")] ctx `shouldBe` lam ["x"] (if' (eq (var "x") (int 1)) (var "x") err) empty
    match [([(PCtr "Unknown" [], "x")], var "x")] ctx `shouldBe` Lam "%0" Err
    match [([(PCtr "A" [], "x")], var "x")] ctx `shouldBe` Lam "%0" (App (App (Var "%0") (App (Lam "x" (Var "x")) (Var "%0"))) Err)
    match [([(PCtr "B" [(PAny, "a")], "x")], var "x")] ctx `shouldBe` Lam "%0" (App (App (Var "%0") Err) (Lam "%1" (App (Lam "x" (Var "x")) (Var "%0"))))

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
