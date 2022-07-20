module ZenTests where

import qualified Core as C
import Test.Hspec
import Zen

ctx :: Context
ctx = defineType "List" [("Nil", []), ("Cons", ["x", "xs"])] empty

zenTests :: SpecWith ()
zenTests = describe "--== Zen language ==--" $ do
  it "☯ app" $ do
    app (Var "x") [] `shouldBe` Var "x"
    app (Var "x") [Var "y"] `shouldBe` App (Var "x") (Var "y")
    app (Var "x") [Var "y", Var "z"] `shouldBe` App (App (Var "x") (Var "y")) (Var "z")

  -- it "☯ lam" $ do
  --   Lam [] (Int 1) `shouldBe` Int 1
  --   Lam ["x"] (Int 1) `shouldBe` Lam ["x"] (Int 1)
  --   Lam ["x", "y"] (Int 1) `shouldBe` Lam ["x"] (Lam "y" (Int 1))

  --   it "☯ built-in operators" $ do
  --     add (Var "x") (Var "y") `shouldBe` app (Op2 Add) [Var "x", Var "y"]
  --     sub (Var "x") (Var "y") `shouldBe` app (Op2 Sub) [Var "x", Var "y"]
  --     mul (Var "x") (Var "y") `shouldBe` app (Op2 Mul) [Var "x", Var "y"]
  --     eq (Var "x") (Var "y") `shouldBe` app (Op2 Eq) [Var "x", Var "y"]

  --   it "☯ if" $ do
  --     if' (Var "x") (Var "y") (Var "z") `shouldBe` app (Var "x") [Var "y", Var "z"]

  it "☯ toCore" $ do
    let ctx = defineType "List" [("Nil", []), ("Cons", ["x", "xs"])] empty
    toCore Err ctx `shouldBe` C.Err
    toCore (Var "x") ctx `shouldBe` C.Var "x"
    toCore (Int 1) ctx `shouldBe` C.Int 1
    toCore (App (Var "x") (Var "y")) ctx `shouldBe` C.App (C.Var "x") (C.Var "y")
    toCore (Lam "x" (Var "y")) ctx `shouldBe` C.Lam "x" (C.Var "y")
    toCore (If (Var "cond") (Var "then") (Var "else")) ctx `shouldBe` C.app (C.Var "cond") [C.Var "then", C.Var "else"]
    toCore (Case (Var "a") [] Err) ctx `shouldBe` C.Err
    toCore (Case (Var "a") [("Unknown", Var "b")] Err) ctx `shouldBe` C.Err
    toCore (Case (Var "a") [("Nil", Var "b")] Err) ctx `shouldBe` C.app (C.Var "a") [C.Var "b", C.Err]
    toCore (Case (Var "a") [("Cons", lam ["y", "ys"] (Var "b"))] Err) ctx `shouldBe` C.app (C.Var "a") [C.Err, C.lam ["y", "ys"] (C.Var "b")]
    toCore (Match [] Err) ctx `shouldBe` C.Err
    toCore (Match [([], Int 1)] Err) ctx `shouldBe` C.Int 1
    toCore (Match [([PAny], Int 1)] Err) ctx `shouldBe` C.Lam "" (C.Int 1)
    toCore (Match [([PVar "x"], Var "x")] Err) ctx `shouldBe` C.Lam "x" (C.Var "x")
    toCore (Match [([PInt 1], Var "x")] Err) ctx `shouldBe` toCore (Lam "_1" (If (eq (Var "_1") (Int 1)) (Var "x") (App Err (Var "_1")))) ctx
    toCore (Match [([PCtr "Nil" []], Var "x")] Err) ctx `shouldBe` C.Lam "" (C.app (C.Var "") [C.Var "x", C.Err])
    toCore (Match [([PCtr "Nil" [PVar "extra"]], Var "x")] Err) ctx `shouldBe` C.Lam "" (C.app (C.Var "") [C.Var "x", C.Err])
    toCore (Match [([PCtr "Cons" [PVar "x", PVar "xs"]], Var "x")] Err) ctx `shouldBe` C.Lam "" (C.app (C.Var "") [C.Err, C.lam ["x", "xs"] (C.Var "x")])
    toCore (Match [([PCtr "Cons" [PVar "y", PVar "xs"]], Var "y")] Err) ctx `shouldBe` C.Lam "" (C.app (C.Var "") [C.Err, C.lam ["x", "xs"] (C.App (C.Lam "y" (C.Var "y")) (C.Var "x"))])
    toCore Add ctx `shouldBe` C.Op2 C.Add
    toCore Sub ctx `shouldBe` C.Op2 C.Sub
    toCore Mul ctx `shouldBe` C.Op2 C.Mul

-- it "☯ matchAny" $ do
--   matchAny "a" ([], Var "a") `shouldBe` Nothing
--   matchAny "a" ([PAny, PVar "ys"], Var "a") `shouldBe` Just ([PVar "ys"], Var "a")
--   matchAny "a" ([PVar "a", PVar "ys"], Var "a") `shouldBe` Just ([PVar "ys"], Var "a")
--   matchAny "a" ([PVar "y", PVar "ys"], Var "y") `shouldBe` Just ([PVar "ys"], App (Lam "y" (Var "y")) (Var "a"))
--   matchAny "a" ([PInt 0, PVar "ys"], Var "a") `shouldBe` Nothing
--   matchAny "a" ([PCtr "Nil" [], PVar "ys"], Var "a") `shouldBe` Nothing

-- it "☯ matchCtr" $ do
--   matchCtr "a" "Cons" ["x", "xs"] ([], Var "a") `shouldBe` Nothing
--   matchCtr "a" "Cons" ["x", "xs"] ([PAny, PVar "ys"], Var "a") `shouldBe` Just ([PVar "x", PVar "xs", PVar "ys"], Var "a")
--   matchCtr "a" "Cons" ["x", "xs"] ([PVar "a", PVar "ys"], Var "a") `shouldBe` Just ([PVar "x", PVar "xs", PVar "ys"], Var "a")
--   matchCtr "a" "Cons" ["x", "xs"] ([PVar "x", PVar "ys"], Var "x") `shouldBe` Just ([PVar "x", PVar "xs", PVar "ys"], App (Lam "x" (Var "x")) (Var "a"))
--   matchCtr "a" "Cons" ["x", "xs"] ([PInt 0, PVar "ys"], Var "a") `shouldBe` Nothing
--   matchCtr "a" "Cons" ["x", "xs"] ([PCtr "Nil" [], PVar "ys"], Var "a") `shouldBe` Nothing
--   matchCtr "a" "Cons" ["x", "xs"] ([PCtr "Cons" [], PVar "ys"], Var "a") `shouldBe` Just ([PVar "x", PVar "xs", PVar "ys"], Var "a")
--   matchCtr "a" "Cons" ["x", "xs"] ([PCtr "Cons" [PVar "x"], PVar "ys"], Var "x") `shouldBe` Just ([PVar "x", PVar "xs", PVar "ys"], Var "x")
--   matchCtr "a" "Cons" ["x", "xs"] ([PCtr "Cons" [PVar "y"], PVar "ys"], Var "y") `shouldBe` Just ([PVar "x", PVar "xs", PVar "ys"], App (Lam "y" (Var "y")) (Var "x"))
--   matchCtr "a" "Cons" ["x", "xs"] ([PCtr "Cons" [PVar "x", PVar "xs"], PVar "ys"], Var "a") `shouldBe` Just ([PVar "x", PVar "xs", PVar "ys"], Var "a")
--   matchCtr "a" "Cons" ["x", "xs"] ([PCtr "Cons" [PVar "a", PVar "bs"], PVar "ys"], Var "a") `shouldBe` Just ([PVar "x", PVar "xs", PVar "ys"], App (Lam "bs" (App (Lam "a" (Var "a")) (Var "x"))) (Var "xs"))
--   matchCtr "a" "Cons" ["x", "xs"] ([PCtr "Cons" [PVar "x", PVar "xs", PVar "z"], PVar "ys"], Var "a") `shouldBe` Just ([PVar "x", PVar "xs", PVar "ys"], Var "a")

--   it "☯ defineType" $ do
--     constructors (defineType "T" [("A", []), ("B", ["x", "y"])] empty)
--       `shouldBe` [("A", [], ["A", "B"]), ("B", ["x", "y"], ["A", "B"])]

--   -- it "☯ defineRule" $ do
--   --   definitions (defineRule "x" [] (Int 1) empty) `shouldBe` [("x", [([], Int 1)])]
--   --   definitions (defineRule "x" [] (Int 1) (defineRule "y" [] (Int 2) empty)) `shouldBe` [("x", [([], Int 1)]), ("y", [([], Int 2)])]

--   -- it "☯ defineRules" $ do
--   --   let ctx = defineRule "x" [] (Int 0) empty
--   --   definitions (defineRules [] ctx) `shouldBe` [("x", [([], Int 0)])]
--   --   definitions (defineRules [("x", [], Int 1)] ctx) `shouldBe` [("x", [([], Int 1), ([], Int 0)])]
--   --   definitions (defineRules [("x", [], Int 1), ("x", [], Int 2)] ctx) `shouldBe` [("x", [([], Int 1), ([], Int 2), ([], Int 0)])]

--   it "☯ constructorAlternatives" $ do
--     constructorAlternatives "Undefined" ctx `shouldBe` Nothing
--     constructorAlternatives "Nil" ctx `shouldBe` Just ["Nil", "Cons"]
--     constructorAlternatives "Cons" ctx `shouldBe` Just ["Nil", "Cons"]

--   it "☯ constructorArguments" $ do
--     constructorArguments "Undefined" ctx `shouldBe` Nothing
--     constructorArguments "Nil" ctx `shouldBe` Just []
--     constructorArguments "Cons" ctx `shouldBe` Just ["x", "xs"]

--   it "☯ caseFind" $ do
--     caseFind [("Nil", [], Int 1), ("Cons", ["x", "xs"], Int 2)] (Int 0) "Unknown" `shouldBe` Int 0
--     caseFind [("Nil", [], Int 1), ("Cons", ["x", "xs"], Int 2)] (Int 0) "Nil" `shouldBe` Int 1
--     caseFind [("Nil", [], Int 1), ("Cons", ["x", "xs"], Int 2)] (Int 0) "Cons" `shouldBe` lam ["x", "xs"] (Int 2)

--   it "☯ case" $ do
--     case' (Var "a") [] (Int 0) ctx `shouldBe` Int 0
--     case' (Var "a") [("Nil", [], Int 1)] (Int 0) ctx `shouldBe` app (Var "a") [Int 1, Int 0]
--     case' (Var "a") [("Nil", [], Int 1), ("Cons", ["x", "xs"], Int 2)] (Int 0) ctx `shouldBe` app (Var "a") [Int 1, lam ["x", "xs"] (Int 2)]
--     case' (Var "a") [("Cons", ["x", "xs"], Int 2), ("Nil", [], Int 1)] (Int 0) ctx `shouldBe` app (Var "a") [Int 1, lam ["x", "xs"] (Int 2)]

--   it "☯ match" $ do
--     -- TODO: make more tests
--     match [] (Int 0) ctx `shouldBe` Int 0
--     match [([], Var "x")] (Int 0) ctx `shouldBe` Var "x"
--     match [([PAny], Var "x")] (Int 0) ctx `shouldBe` Lam "x1" (Var "x")
--     match [([PAny], Var "y")] (Int 0) ctx `shouldBe` Lam "x" (Var "y")
--     match [([PVar "x"], Var "x")] (Int 0) ctx `shouldBe` Lam "x" (Var "x")
--     match [([PVar "y"], Var "x")] (Int 0) ctx `shouldBe` Lam "y" (Var "x")
--     match [([PCtr "Unknown" []], Var "x")] (Int 0) ctx `shouldBe` Lam "x1" (Int 0)
--     match [([PCtr "Nil" []], Var "x")] (Int 0) ctx `shouldBe` Lam "x1" (app (Var "x1") [Var "x", Int 0])
--     match [([PCtr "Cons" [PVar "x", PVar "xs"]], Var "x")] (Int 0) ctx `shouldBe` Lam "x" (app (Var "x") [Int 0, lam ["x", "xs"] (Var "x")])
--     match [([PCtr "Cons" [PVar "xs", PVar "x"]], Var "xs")] (Int 0) ctx `shouldBe` Lam "x" (app (Var "x") [Int 0, lam ["xs", "x"] (Var "xs")])

--   it "☯ nameIndex" $ do
--     nameIndex "" "" `shouldBe` Nothing
--     nameIndex "" "x" `shouldBe` Nothing
--     nameIndex "" "42" `shouldBe` Just 42
--     nameIndex "x" "x42" `shouldBe` Just 42
--     nameIndex "x" "y42" `shouldBe` Nothing

--   it "☯ findLastNameIndex" $ do
--     findLastNameIndex "x" [] `shouldBe` Nothing
--     findLastNameIndex "x" ["x"] `shouldBe` Just 0
--     findLastNameIndex "x" ["x1"] `shouldBe` Just 1
--     findLastNameIndex "x" ["x", "x1"] `shouldBe` Just 1
--     findLastNameIndex "x" ["x1", "x"] `shouldBe` Just 1
--     findLastNameIndex "x" ["x1", "x2"] `shouldBe` Just 2
--     findLastNameIndex "x" ["x2", "x1"] `shouldBe` Just 2

--   it "☯ freeVariables" $ do
--     freeVariables (Var "x") `shouldBe` ["x"]
--     freeVariables (Int 1) `shouldBe` []
--     freeVariables (App (Var "x") (Var "x")) `shouldBe` ["x"]
--     freeVariables (App (Var "x") (Var "y")) `shouldBe` ["x", "y"]
--     freeVariables (Lam "x" (Var "x")) `shouldBe` []
--     freeVariables (Lam "x" (Var "y")) `shouldBe` ["y"]
--     freeVariables (Op2 Add) `shouldBe` []

--   it "☯ newName" $ do
--     newName [] "x" `shouldBe` "x"
--     newName ["x"] "x" `shouldBe` "x1"
--     newName ["x", "x1"] "x" `shouldBe` "x2"

--   it "☯ newNames" $ do
--     newNames [] [] `shouldBe` []
--     newNames [] ["x"] `shouldBe` ["x"]
--     newNames ["x"] ["x"] `shouldBe` ["x1"]
--     newNames ["x"] ["x", "x"] `shouldBe` ["x1", "x2"]
--     newNames ["x3"] ["x", "x"] `shouldBe` ["x4", "x5"]

--   describe "☯ parse" $ do
--     it "☯ basic" $ do
--       parse "x" `shouldBe` Right (Var "x")
--       parse "42" `shouldBe` Right (Int 42)
--       parse "\\ -> 42" `shouldBe` Right (Int 42)
--       parse "\\x -> 42" `shouldBe` Right (Lam "x" (Int 42))
--       parse "\\x y z -> 42" `shouldBe` Right (lam ["x", "y", "z"] (Int 42))
--       parse "(+)" `shouldBe` Right (Op2 Add)
--       parse "(-)" `shouldBe` Right (Op2 Sub)
--       parse "(*)" `shouldBe` Right (Op2 Mul)
--       parse "( + )" `shouldBe` Right (Op2 Add)
--       parse "-- Comment\nx" `shouldBe` Right (Var "x")

--     it "☯ operator precedence" $ do
--       let (x, y, z) = (Var "x", Var "y", Var "z")
--       parse "x + y + z" `shouldBe` Right (add (add x y) z)
--       parse "x + y - z" `shouldBe` Right (sub (add x y) z)
--       parse "x + y * z" `shouldBe` Right (add x (mul y z))
--       parse "x - y + z" `shouldBe` Right (add (sub x y) z)
--       parse "x - y - z" `shouldBe` Right (sub (sub x y) z)
--       parse "x - y * z" `shouldBe` Right (sub x (mul y z))
--       parse "x * y + z" `shouldBe` Right (add (mul x y) z)
--       parse "x * y - z" `shouldBe` Right (sub (mul x y) z)
--       parse "x * y * z" `shouldBe` Right (mul (mul x y) z)
--       parse "x * y z" `shouldBe` Right (mul x (App y z))
--       parse "x y * z" `shouldBe` Right (mul (App x y) z)
--       parse "x y z" `shouldBe` Right (App (App x y) z)

-- -- it "☯ let binding" $ do
-- --   parse "@x = 1" `shouldBe` Right ("x", Int 1)