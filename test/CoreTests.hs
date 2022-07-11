module CoreTests where

import Core
import Test.Hspec

ctx :: Context
ctx "Nil" = Just ["Nil", "Cons"]
ctx "Cons" = Just ["Nil", "Cons"]
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

  it "☯ case" $ do
    case' (Var "a") [] (Int 0) ctx `shouldBe` Int 0
    case' (Var "a") [("Nil", [], Int 1)] (Int 0) ctx `shouldBe` app (Var "a") [Int 1, Int 0]
    case' (Var "a") [("Nil", [], Int 1), ("Cons", ["x", "xs"], Int 2)] (Int 0) ctx `shouldBe` app (Var "a") [Int 1, lam ["x", "xs"] (Int 2)]
    case' (Var "a") [("Cons", ["x", "xs"], Int 2), ("Nil", [], Int 1)] (Int 0) ctx `shouldBe` app (Var "a") [Int 1, lam ["x", "xs"] (Int 2)]

  it "☯ match" $ do
    match [] [] (Int 0) ctx `shouldBe` Int 0
    match [] [([], Var "x")] (Int 0) ctx `shouldBe` Var "x"
    match [Var "a"] [] (Int 0) ctx `shouldBe` Lam "" (Int 0)
    match [Var "a", Var "b"] [] (Int 0) ctx `shouldBe` lam ["", ""] (Int 0)
    match [Var "a"] [([PAny], Var "x")] (Int 0) ctx `shouldBe` Var "x"
    match [Var "a"] [([PVar "x"], Var "x")] (Int 0) ctx `shouldBe` Var "a"
    match [Var "a"] [([PCtr "Unknown" []], Var "x")] (Int 0) ctx `shouldBe` Int 0
    match [Var "a"] [([PCtr "Nil" []], Var "x")] (Int 0) ctx `shouldBe` app (Var "a") [Var "x", Int 0]
    match [Var "a"] [([PCtr "Cons" [PVar "x", PVar "xs"]], Var "x")] (Int 0) ctx `shouldBe` app (Var "a") [Int 0, lam ["%1", "%2"] (Var "%1")]
    match [Var "a"] [([PCtr "Cons" [PVar "xs", PVar "x"]], Var "xs")] (Int 0) ctx `shouldBe` app (Var "a") [Int 0, lam ["%1", "%2"] (Var "%1")]
    True `shouldBe` True

-- it "☯ matchToCase" $ do
--   matchToCase [] [] (Int 0) `shouldBe` Int 0
--   matchToCase [] [([PAny], Var "x")] (Int 0) `shouldBe` Var "x"
--   matchToCase [Int 1, Int 2] [([PAny, PVar "y"], Var "x"), ([PAny, PVar "y"], Var "y")] (Int 0) `shouldBe` Case (Int 1) [] (Match [Int 2] [([PVar "y"], Var "x"), ([PVar "y"], Var "y")] (Int 0))
--   matchToCase [Int 1, Int 2] [([PVar "x", PVar "y"], Var "x"), ([PVar "x", PVar "y"], Var "y")] (Int 0) `shouldBe` Case (Int 1) [] (Match [Int 2] [([PVar "y"], Int 1), ([PVar "y"], Var "y")] (Int 0))
--   -- matchToCase [Int 1, Int 2] [([PCtr "Nil" [], PVar "y"], Var "x"), ([PCtr "Cons" [PVar "x1", PVar "x2"], PVar "y"], Var "y")] (Int 0)
--   --   `shouldBe` Case (Int 1) [("Nil", Match [Int 2] [([PVar "y"], Var "x")] (Int 0)), ("Cons", Lam ["x", "xs"] (Match [Var "x", Var "xs", Int 2] [([PVar "x1", PVar "x2", PVar "y"], Var "y")] (Int 0)))] (Match [Int 2] [] (Int 0))
--   matchToCase [Int 1, Int 2] [([PInt 3, PVar "y"], Var "x"), ([PInt 4, PVar "y"], Var "y")] (Int 0)
--     `shouldBe` If (eq (Int 1) (Int 3)) (Match [Int 2] [([PVar "y"], Var "x")] (Int 0)) (Match [Int 1, Int 2] [([PInt 4, PVar "y"], Var "y")] (Int 0))

--   prepend 0 '*' "abc" `shouldBe` "abc"
--   prepend 1 '*' "abc" `shouldBe` "*abc"
--   prepend 2 '*' "abc" `shouldBe` "**abc"

-- it "☯ resize" $ do
--   resize 0 '*' "abc" `shouldBe` ""
--   resize 1 '*' "abc" `shouldBe` "a"
--   resize 2 '*' "abc" `shouldBe` "ab"
--   resize 3 '*' "abc" `shouldBe` "abc"
--   resize 4 '*' "abc" `shouldBe` "abc*"
--   resize 5 '*' "abc" `shouldBe` "abc**"

-- it "☯ findTempVar" $ do
--   findTempVar "" (Var "") `shouldBe` Nothing
--   findTempVar "" (Var "42") `shouldBe` Just 42
--   findTempVar "x" (Var "y42") `shouldBe` Nothing
--   findTempVar "x" (Var "x42") `shouldBe` Just 42
--   findTempVar "" (App (Var "1") []) `shouldBe` Just 1
--   findTempVar "" (App (Var "2") [Var "1"]) `shouldBe` Just 2
--   findTempVar "" (App (Var "1") [Var "2"]) `shouldBe` Just 2
--   findTempVar "" (App (Var "1") [Var "2", Var "3"]) `shouldBe` Just 3
--   findTempVar "" (Lam [] (Var "1")) `shouldBe` Just 1
--   findTempVar "" (Lam ["2"] (Var "1")) `shouldBe` Just 2
--   findTempVar "" (Lam ["1"] (Var "2")) `shouldBe` Just 2
--   findTempVar "" (Lam ["1", "2"] (Var "3")) `shouldBe` Just 3

-- it "☯ findCtrs" $ do
--   findCtrs [] `shouldBe` []
--   findCtrs [([PAny], Int 1)] `shouldBe` []
--   findCtrs [([PVar "x"], Int 1)] `shouldBe` []
--   findCtrs [([PCtr "A" []], Int 1)] `shouldBe` [("A", 0)]
--   findCtrs [([PCtr "A" []], Int 1), ([PCtr "B" [PAny, PAny]], Int 2)] `shouldBe` [("A", 0), ("B", 2)]

-- -- TODO: test casePop
-- -- TODO: test caseToCore
-- it "☯ matchAny" $ do
--   matchAny (Int 0) [] `shouldBe` []
--   matchAny (Int 0) [([PAny, PVar "y"], Var "x")] `shouldBe` [([PVar "y"], Var "x")]
--   matchAny (Int 0) [([PVar "x", PVar "y"], Var "x")] `shouldBe` [([PVar "y"], Int 0)]
--   matchAny (Int 0) [([PCtr "A" [], PVar "y"], Var "x")] `shouldBe` []
--   matchAny (Int 0) [([PInt 1, PVar "y"], Var "x")] `shouldBe` []

-- it "☯ matchToCase" $ do
--   matchToCase [] [] (Int 0) `shouldBe` Int 0
--   matchToCase [] [([PAny], Var "x")] (Int 0) `shouldBe` Var "x"
--   matchToCase [Int 1, Int 2] [([PAny, PVar "y"], Var "x"), ([PAny, PVar "y"], Var "y")] (Int 0) `shouldBe` Case (Int 1) [] (Match [Int 2] [([PVar "y"], Var "x"), ([PVar "y"], Var "y")] (Int 0))
--   matchToCase [Int 1, Int 2] [([PVar "x", PVar "y"], Var "x"), ([PVar "x", PVar "y"], Var "y")] (Int 0) `shouldBe` Case (Int 1) [] (Match [Int 2] [([PVar "y"], Int 1), ([PVar "y"], Var "y")] (Int 0))
--   -- matchToCase [Int 1, Int 2] [([PCtr "Nil" [], PVar "y"], Var "x"), ([PCtr "Cons" [PVar "x1", PVar "x2"], PVar "y"], Var "y")] (Int 0)
--   --   `shouldBe` Case (Int 1) [("Nil", Match [Int 2] [([PVar "y"], Var "x")] (Int 0)), ("Cons", Lam ["x", "xs"] (Match [Var "x", Var "xs", Int 2] [([PVar "x1", PVar "x2", PVar "y"], Var "y")] (Int 0)))] (Match [Int 2] [] (Int 0))
--   matchToCase [Int 1, Int 2] [([PInt 3, PVar "y"], Var "x"), ([PInt 4, PVar "y"], Var "y")] (Int 0)
--     `shouldBe` If (eq (Int 1) (Int 3)) (Match [Int 2] [([PVar "y"], Var "x")] (Int 0)) (Match [Int 1, Int 2] [([PInt 4, PVar "y"], Var "y")] (Int 0))

-- it "☯ toCore" $ do
--   toCore ctx (Var "x") `shouldBe` C.Var "x"
--   toCore ctx (Int 1) `shouldBe` C.Int 1
--   toCore ctx (Ctr "Undefined") `shouldBe` C.Var "Undefined"
--   toCore ctx (Ctr "Nil") `shouldBe` C.lam ["Nil", "Cons"] (C.Var "Nil")
--   toCore ctx (Ctr "Cons") `shouldBe` C.lam ["x", "xs", "Nil", "Cons"] (C.app (C.Var "Cons") [C.Var "x", C.Var "xs"])
--   toCore ctx (App (Var "x") []) `shouldBe` C.Var "x"
--   toCore ctx (App (Var "x") [Var "y"]) `shouldBe` C.App (C.Var "x") (C.Var "y")
--   toCore ctx (App (Var "x") [Var "y", Var "z"]) `shouldBe` C.app (C.Var "x") [C.Var "y", C.Var "z"]
--   toCore ctx (Lam [] (Int 0)) `shouldBe` C.Int 0
--   toCore ctx (Lam ["x"] (Int 0)) `shouldBe` C.Lam "x" (C.Int 0)
--   toCore ctx (Lam ["x", "y", "z"] (Int 0)) `shouldBe` C.lam ["x", "y", "z"] (C.Int 0)
--   toCore ctx (If (Var "x") (Var "y") (Var "z")) `shouldBe` C.app (C.Var "x") [C.Var "y", C.Var "z"]
--   toCore ctx (Case (Var "x") [] (Int 0)) `shouldBe` C.Int 0
--   toCore ctx (Case (Var "x") [("Undefined", Int 1)] (Int 0)) `shouldBe` C.Int 0
--   -- toCore ctx (Case (Var "x") [("A", Int 1)] (Int 0)) `shouldBe` C.app (C.Var "x") [C.Int 1, C.Int 0, C.Int 0]
--   -- toCore ctx (Case (Var "x") [("A", Int 1), ("B", Int 2)] (Int 0)) `shouldBe` C.app (C.Var "x") [C.Int 1, C.Int 2, C.Int 0]
--   -- toCore ctx (Case (Var "x") [("A", Int 1), ("B", Int 2), ("C", Int 3)] (Int 0)) `shouldBe` C.app (C.Var "x") [C.Int 1, C.Int 2, C.Int 3]
--   -- toCore ctx (Case (Var "x") [("B", Int 2), ("A", Int 1), ("C", Int 3)] (Int 0)) `shouldBe` C.app (C.Var "x") [C.Int 1, C.Int 2, C.Int 3]
--   -- toCore ctx (Case (Var "x") [("B", Int 2), ("C", Int 3), ("A", Int 1)] (Int 0)) `shouldBe` C.app (C.Var "x") [C.Int 1, C.Int 2, C.Int 3]
--   -- toCore ctx (Case (Var "x") [("Just", Lam ["y"] (Int 2)), ("Nothing", Int 1)] (Int 0)) `shouldBe` C.app (C.Var "x") [C.Int 1, C.Lam "y" (C.Int 2)]
--   toCore ctx (Match [] [] (Int 0)) `shouldBe` C.Int 0
--   toCore ctx (Match [] [([PAny], Var "x")] (Int 0)) `shouldBe` C.Var "x"
--   toCore ctx (Match [Var "a", Var "b"] [([PAny, PVar "y"], Var "x")] (Int 0)) `shouldBe` C.Var "x"
--   toCore ctx (Match [Var "a", Var "b"] [([PVar "x", PVar "y"], Var "x")] (Int 0)) `shouldBe` C.Var "a"
--   -- toCore ctx (Match [Var "a", Var "b"] [([PCtr "Nothing" [], PVar "y"], Var "x")] (Int 0)) `shouldBe` C.Var "a"
--   -- toCore ctx (Match [Var "a", Var "b"] [([PCtr "Just" [PVar "x"], PVar "y"], Var "x")] (Int 0)) `shouldBe` C.Var "a"
--   -- matchToCase [Int 1, Int 2] [([PAny, PVar "y"], Var "x"), ([PAny, PVar "y"], Var "y")] (Int 0) `shouldBe` Case (Int 1) [] (Match [Int 2] [([PVar "y"], Var "x"), ([PVar "y"], Var "y")] (Int 0))
--   -- matchToCase [Int 1, Int 2] [([PVar "x", PVar "y"], Var "x"), ([PVar "x", PVar "y"], Var "y")] (Int 0) `shouldBe` Case (Int 1) [] (Match [Int 2] [([PVar "y"], Int 1), ([PVar "y"], Var "y")] (Int 0))
--   -- matchToCase [Int 1, Int 2] [([PCtr "A" [], PVar "y"], Var "x"), ([PCtr "B" [PVar "x1", PVar "x2"], PVar "y"], Var "y")] (Int 0)
--   --   `shouldBe` Case (Int 1) [("A", Match [Int 2] [([PVar "y"], Var "x")] (Int 0)), ("B", Match [Int 2] [([PVar "x1", PVar "x2", PVar "y"], Var "y")] (Int 0))] (Match [Int 2] [] (Int 0))
--   -- matchToCase [Int 1, Int 2] [([PInt 3, PVar "y"], Var "x"), ([PInt 4, PVar "y"], Var "y")] (Int 0)
--   --   `shouldBe` If (eq (Int 1) (Int 3)) (Match [Int 2] [([PVar "y"], Var "x")] (Int 0)) (Match [Int 1, Int 2] [([PInt 4, PVar "y"], Var "y")] (Int 0))

--   -- toCore ctx (Match [] [([PVar "x"], Int 1), ([], Int 2)] (Int 0)) `shouldBe` C.Int 1
--   -- toCore ctx (Match [Var "a"] [] (Int 0)) `shouldBe` C.Int 0
--   -- toCore ctx (Match [Var "a"] [([PVar "x"], Var "x")] (Int 0)) `shouldBe` C.Var "a"
--   -- toCore ctx (Match [Var "a"] [([PVar "x"], Var "x"), ([PVar "y"], Var "y")] (Int 0)) `shouldBe` C.Var "a"
--   -- toCore ctx (Match [Var "a"] [([PCtr "A" []], Var "x")] (Int 0)) `shouldBe` C.Var "a"
--   -- toCore ctx (Match [Var "a"] [([PCtr "Just" [PVar "x"]], Var "x")] (Int 0)) `shouldBe` C.Var "a"
--   -- toCore ctx (Match [Var "a"] [([PCtr "Just" [PVar "x"]], Var "x"), ([PVar "y"], Var "y")] (Int 0)) `shouldBe` C.Var "a"
--   toCore ctx (Op2 C.Add) `shouldBe` C.Op2 C.Add
--   toCore ctx (Op2 C.Sub) `shouldBe` C.Op2 C.Sub
