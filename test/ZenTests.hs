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

  describe "☯ parse" $ do
    it "☯ basic" $ do
      parse "x" `shouldBe` Right (Var "x")
      parse "42" `shouldBe` Right (Int 42)
      parse "\\ -> 42" `shouldBe` Right (Int 42)
      parse "\\x -> 42" `shouldBe` Right (Lam "x" (Int 42))
      parse "\\x y z -> 42" `shouldBe` Right (lam ["x", "y", "z"] (Int 42))
      parse "(+)" `shouldBe` Right Add
      parse "(-)" `shouldBe` Right Sub
      parse "(*)" `shouldBe` Right Mul
      parse "( + )" `shouldBe` Right Add
      parse "-- Comment\nx" `shouldBe` Right (Var "x")

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

-- -- it "☯ let binding" $ do
-- --   parse "@x = 1" `shouldBe` Right ("x", Int 1)