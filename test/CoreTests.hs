module CoreTests where

import Core
import Test.Hspec

ctx :: Context
ctx "Nil" = Just [("Nil", []), ("Cons", ["x", "xs"])]
ctx "Cons" = Just [("Nil", []), ("Cons", ["x", "xs"])]
ctx _ = Nothing

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

  it "☯ ctrAlts" $ do
    ctrAlts "Undefined" ctx `shouldBe` Nothing
    ctrAlts "Nil" ctx `shouldBe` Just ["Nil", "Cons"]
    ctrAlts "Cons" ctx `shouldBe` Just ["Nil", "Cons"]

  it "☯ ctrArgs" $ do
    ctrArgs "Undefined" ctx `shouldBe` Nothing
    ctrArgs "Nil" ctx `shouldBe` Just []
    ctrArgs "Cons" ctx `shouldBe` Just ["x", "xs"]

  it "☯ caseFind" $ do
    caseFind [("Nil", [], Int 1), ("Cons", ["x", "xs"], Int 2)] (Int 0) "Unknown" `shouldBe` Int 0
    caseFind [("Nil", [], Int 1), ("Cons", ["x", "xs"], Int 2)] (Int 0) "Nil" `shouldBe` Int 1
    caseFind [("Nil", [], Int 1), ("Cons", ["x", "xs"], Int 2)] (Int 0) "Cons" `shouldBe` lam ["x", "xs"] (Int 2)

  it "☯ case" $ do
    case' (Var "a") [] (Int 0) ctx `shouldBe` Int 0
    case' (Var "a") [("Nil", [], Int 1)] (Int 0) ctx `shouldBe` app (Var "a") [Int 1, Int 0]
    case' (Var "a") [("Nil", [], Int 1), ("Cons", ["x", "xs"], Int 2)] (Int 0) ctx `shouldBe` app (Var "a") [Int 1, lam ["x", "xs"] (Int 2)]
    case' (Var "a") [("Cons", ["x", "xs"], Int 2), ("Nil", [], Int 1)] (Int 0) ctx `shouldBe` app (Var "a") [Int 1, lam ["x", "xs"] (Int 2)]

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

  it "☯ renamePatterns" $ do
    renamePatterns [PAny] ["y"] (Var "x") `shouldBe` Var "x"
    renamePatterns [PVar "x"] ["y"] (Var "x") `shouldBe` Var "y"
    renamePatterns [PVar "x"] ["x"] (Var "x") `shouldBe` Var "x"
    renamePatterns [PAny, PVar "x"] ["y", "z"] (Var "x") `shouldBe` Var "z"

  it "☯ pathToCase" $ do
    pathToCase ctx ([], Int 0) `shouldBe` Nothing
    pathToCase ctx ([PAny], Int 0) `shouldBe` Nothing
    pathToCase ctx ([PVar "x"], Int 0) `shouldBe` Nothing
    pathToCase ctx ([PInt 1], Int 0) `shouldBe` Nothing
    pathToCase ctx ([PCtr "Unknown" []], Int 0) `shouldBe` Nothing
    pathToCase ctx ([PCtr "Nil" []], Int 0) `shouldBe` Just ("Nil", [], Int 0)
    pathToCase ctx ([PCtr "Cons" [PVar "a", PAny]], Var "a") `shouldBe` Just ("Cons", ["a", "xs"], Var "a")
    pathToCase ctx ([PCtr "Cons" [PVar "x", PAny]], Var "x") `shouldBe` Just ("Cons", ["x", "xs"], Var "x")
    pathToCase ctx ([PCtr "Cons" [PVar "x", PAny]], Var "xs") `shouldBe` Just ("Cons", ["x", "xs1"], Var "xs")

  it "☯ match" $ do
    match [] [] (Int 0) ctx `shouldBe` Int 0
    match [] [([], Var "x")] (Int 0) ctx `shouldBe` Var "x"
    match [Var "a"] [] (Int 0) ctx `shouldBe` Lam "" (Int 0)
    match [Var "a", Var "b"] [] (Int 0) ctx `shouldBe` lam ["", ""] (Int 0)
    match [Var "a"] [([PAny], Var "x")] (Int 0) ctx `shouldBe` Var "x"
    match [Var "a"] [([PVar "x"], Var "x")] (Int 0) ctx `shouldBe` Var "a"
    match [Var "a"] [([PCtr "Unknown" []], Var "x")] (Int 0) ctx `shouldBe` Int 0
    match [Var "a"] [([PCtr "Nil" []], Var "x")] (Int 0) ctx `shouldBe` app (Var "a") [Var "x", Int 0]
    match [Var "a"] [([PCtr "Cons" [PVar "x", PVar "xs"]], Var "x")] (Int 0) ctx `shouldBe` app (Var "a") [Int 0, lam ["x", "xs"] (Var "x")]
    match [Var "a"] [([PCtr "Cons" [PVar "xs", PVar "x"]], Var "xs")] (Int 0) ctx `shouldBe` app (Var "a") [Int 0, lam ["xs", "x"] (Var "xs")]

  describe "☯ parse" $ do
    it "☯ basic" $ do
      parse "x" `shouldBe` Right (Var "x")
      parse "42" `shouldBe` Right (Int 42)
      parse "\\ -> 42" `shouldBe` Right (Int 42)
      parse "\\x -> 42" `shouldBe` Right (Lam "x" (Int 42))
      parse "\\x y z -> 42" `shouldBe` Right (lam ["x", "y", "z"] (Int 42))
      parse "x y" `shouldBe` Right (App (Var "x") (Var "y"))
      parse "(+)" `shouldBe` Right (Op2 Add)
      parse "(-)" `shouldBe` Right (Op2 Sub)
      parse "(*)" `shouldBe` Right (Op2 Mul)
      parse "( + )" `shouldBe` Right (Op2 Add)

    it "☯ operator precedence" $ do
      let (x, y, z) = (Var "x", Var "y", Var "z")
      parse "x + y + z" `shouldBe` Right (add (add x y) z)
      parse "x + y - z" `shouldBe` Right (sub (add x y) z)
      parse "x + y * z" `shouldBe` Right (add x (mul y z))
      parse "x - y + z" `shouldBe` Right (add (sub x y) z)
      parse "x - y - z" `shouldBe` Right (sub (sub x y) z)
      parse "x - y * z" `shouldBe` Right (sub x (mul y z))
      parse "x * y + z" `shouldBe` Right (add (mul x y) z)
      parse "x * y - z" `shouldBe` Right (sub (mul x y) z)
      parse "x * y * z" `shouldBe` Right (mul (mul x y) z)
      parse "x * y z" `shouldBe` Right (mul x (App y z))
      parse "x y * z" `shouldBe` Right (mul (App x y) z)
      parse "x y z" `shouldBe` Right (App (App x y) z)
