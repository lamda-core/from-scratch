module ZenTests where

import Core
import Test.Hspec
import Zen

zenTests :: SpecWith ()
zenTests = describe "--== Zen language ==--" $ do
  it "☯ basic" $ do
    parse "x" `shouldBe` Right (Var "x")
    parse "42" `shouldBe` Right (Int 42)
    -- parse "\\ -> 42" `shouldBe` Right (Int 42)
    -- parse "\\x -> 42" `shouldBe` Right (Lam "x" (Int 42))
    -- parse "\\x y z -> 42" `shouldBe` Right (lam ["x", "y", "z"] (Int 42))
    parse "(+)" `shouldBe` Right (Op2 Add)
    parse "(-)" `shouldBe` Right (Op2 Sub)
    parse "(*)" `shouldBe` Right (Op2 Mul)
    parse "( + )" `shouldBe` Right (Op2 Add)
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