module ZenTests where

import Core
import Parser (parse)
import Test.Hspec
import Zen

zenTests :: SpecWith ()
zenTests = describe "--== Zen language ==--" $ do
  it "☯ variable" $ do
    parse "a" variable `shouldBe` Right "a"
    parse "a1" variable `shouldBe` Right "a1"

  it "☯ constructor" $ do
    parse "A" constructor `shouldBe` Right "A"
    parse "A1" constructor `shouldBe` Right "A1"

  it "☯ comment" $ do
    parse "--my comment" comment `shouldBe` Right "my comment"
    parse "-- my comment" comment `shouldBe` Right "my comment"
    parse "--  my  comment  \nx" comment `shouldBe` Right " my  comment  "

  it "☯ binding" $ do
    parse "_" binding `shouldBe` Right (PAny, "")
    parse "_" binding `shouldBe` Right (PAny, "")
    parse "42" binding `shouldBe` Right (PInt 42, "")
    parse "True" binding `shouldBe` Right (PCtr "True" [], "")
    parse "x@_" binding `shouldBe` Right (PAny, "x")
    parse "x@42" binding `shouldBe` Right (PInt 42, "x")
    parse "x@True" binding `shouldBe` Right (PCtr "True" [], "x")
    parse "x" binding `shouldBe` Right (PAny, "x")

  it "☯ pattern" $ do
    parse "_" pattern `shouldBe` Right PAny
    parse "42" pattern `shouldBe` Right (PInt 42)
    parse "True" pattern `shouldBe` Right (PCtr "True" [])
    parse "Cons 1 xs" pattern `shouldBe` Right (PCtr "Cons" [(PInt 1, ""), (PAny, "xs")])
    parse "(Cons 1 xs)" pattern `shouldBe` Right (PCtr "Cons" [(PInt 1, ""), (PAny, "xs")])

  it "☯ case" $ do
    parse "| x -> y" case' `shouldBe` Right ([(PAny, "x")], Var "y")
    parse "| x y -> z" case' `shouldBe` Right ([(PAny, "x"), (PAny, "y")], Var "z")

  it "☯ expression" $ do
    parse "_" expression `shouldBe` Right Err
    parse "x" expression `shouldBe` Right (Var "x")
    parse "42" expression `shouldBe` Right (Int 42)
    parse "\\x. y" expression `shouldBe` Right (Lam "x" (Var "y"))
    parse "\\x y. z" expression `shouldBe` Right (lam ["x", "y"] (Var "z"))
    parse "(+)" expression `shouldBe` Right (Op2 Add)
    parse "(-)" expression `shouldBe` Right (Op2 Sub)
    parse "(*)" expression `shouldBe` Right (Op2 Mul)
    parse "(==)" expression `shouldBe` Right (Op2 Eq)
    parse "-- comment\nx" expression `shouldBe` Right (Var "x")
    parse "x + y" expression `shouldBe` Right (add (Var "x") (Var "y"))
    parse "x - y" expression `shouldBe` Right (sub (Var "x") (Var "y"))
    parse "x * y" expression `shouldBe` Right (mul (Var "x") (Var "y"))
    parse "x == y" expression `shouldBe` Right (eq (Var "x") (Var "y"))
    parse "(1 + 2)" expression `shouldBe` Right (add (Int 1) (Int 2))

  it "☯ operator precedence" $ do
    let (x, y, z) = (Var "x", Var "y", Var "z")
    parse "x == y == z" expression `shouldBe` Right (eq (eq x y) z)
    parse "x == y + z" expression `shouldBe` Right (eq x (add y z))
    parse "x + y == z" expression `shouldBe` Right (eq (add x y) z)
    parse "x + y + z" expression `shouldBe` Right (add (add x y) z)
    parse "x + y - z" expression `shouldBe` Right (sub (add x y) z)
    parse "x + y * z" expression `shouldBe` Right (add x (mul y z))
    parse "x - y + z" expression `shouldBe` Right (add (sub x y) z)
    parse "x - y - z" expression `shouldBe` Right (sub (sub x y) z)
    parse "x - y * z" expression `shouldBe` Right (sub x (mul y z))
    parse "x * y + z" expression `shouldBe` Right (add (mul x y) z)
    parse "x * y - z" expression `shouldBe` Right (sub (mul x y) z)
    parse "x * y * z" expression `shouldBe` Right (mul (mul x y) z)
    parse "x * y z" expression `shouldBe` Right (mul x (App y z))
    parse "x y * z" expression `shouldBe` Right (mul (App x y) z)
    parse "x y z" expression `shouldBe` Right (App (App x y) z)
