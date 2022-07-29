module ZenTests where

import Core
import Parser (parse)
import Test.Hspec
import Zen

zenTests :: SpecWith ()
zenTests = describe "--== Zen language ==--" $ do
  -- it "☯ variableName" $ do
  --   parse "a" variableName `shouldBe` Right "a"
  --   parse "a1" variableName `shouldBe` Right "a1"

  -- it "☯ constructorName" $ do
  --   parse "A" constructorName `shouldBe` Right "A"
  --   parse "A1" constructorName `shouldBe` Right "A1"

  -- it "☯ comment" $ do
  --   parse "--my comment" comment `shouldBe` Right "my comment"
  --   parse "-- my comment" comment `shouldBe` Right "my comment"
  --   parse "--  my  comment  \nx" comment `shouldBe` Right " my  comment  "

  -- it "☯ binding" $ do
  --   parse "_" binding `shouldBe` Right (PAny, "")
  --   parse "_" binding `shouldBe` Right (PAny, "")
  --   parse "42" binding `shouldBe` Right (PInt 42, "")
  --   parse "True" binding `shouldBe` Right (PCtr "True" [], "")
  --   parse "x@_" binding `shouldBe` Right (PAny, "x")
  --   parse "x@42" binding `shouldBe` Right (PInt 42, "x")
  --   parse "x@True" binding `shouldBe` Right (PCtr "True" [], "x")
  --   parse "x" binding `shouldBe` Right (PAny, "x")

  -- it "☯ pattern" $ do
  --   parse "_" pattern `shouldBe` Right PAny
  --   parse "42" pattern `shouldBe` Right (PInt 42)
  --   parse "True" pattern `shouldBe` Right (PCtr "True" [])
  --   parse "Cons 1 xs" pattern `shouldBe` Right (PCtr "Cons" [(PInt 1, ""), (PAny, "xs")])
  --   parse "(Cons 1 xs)" pattern `shouldBe` Right (PCtr "Cons" [(PInt 1, ""), (PAny, "xs")])

  -- it "☯ case" $ do
  --   parse "| x -> y" case' `shouldBe` Right ([(PAny, "x")], Var "y")
  --   parse "| x y -> z" case' `shouldBe` Right ([(PAny, "x"), (PAny, "y")], Var "z")

  -- it "☯ term" $ do
  --   parse "_" term `shouldBe` Right Err
  --   parse "x" term `shouldBe` Right (Var "x")
  --   parse "42" term `shouldBe` Right (Int 42)
  --   parse "\\x. y" term `shouldBe` Right (Lam "x" (Var "y"))
  --   parse "\\x y. z" term `shouldBe` Right (lam ["x", "y"] (Var "z"))
  --   parse "(+)" term `shouldBe` Right (Op2 Add)
  --   parse "(-)" term `shouldBe` Right (Op2 Sub)
  --   parse "(*)" term `shouldBe` Right (Op2 Mul)
  --   parse "(==)" term `shouldBe` Right (Op2 Eq)
  --   parse "(x)" term `shouldBe` Right (Var "x")
  --   parse "-- comment\nx" term `shouldBe` Right (Var "x") -- TODO: move comment into expression or definition
  --   parse "x + y" term `shouldBe` Right (add (Var "x") (Var "y"))
  --   parse "x - y" term `shouldBe` Right (sub (Var "x") (Var "y"))
  --   parse "x * y" term `shouldBe` Right (mul (Var "x") (Var "y"))
  --   parse "x == y" term `shouldBe` Right (eq (Var "x") (Var "y"))

  -- it "☯ operator precedence" $ do
  --   let (x, y, z) = (Var "x", Var "y", Var "z")
  --   parse "x == y == z" term `shouldBe` Right (eq (eq x y) z)
  --   parse "x == y + z" term `shouldBe` Right (eq x (add y z))
  --   parse "x + y == z" term `shouldBe` Right (eq (add x y) z)
  --   parse "x + y + z" term `shouldBe` Right (add (add x y) z)
  --   parse "x + y - z" term `shouldBe` Right (sub (add x y) z)
  --   parse "x + y * z" term `shouldBe` Right (add x (mul y z))
  --   parse "x - y + z" term `shouldBe` Right (add (sub x y) z)
  --   parse "x - y - z" term `shouldBe` Right (sub (sub x y) z)
  --   parse "x - y * z" term `shouldBe` Right (sub x (mul y z))
  --   parse "x * y + z" term `shouldBe` Right (add (mul x y) z)
  --   parse "x * y - z" term `shouldBe` Right (sub (mul x y) z)
  --   parse "x * y * z" term `shouldBe` Right (mul (mul x y) z)
  --   parse "x * y z" term `shouldBe` Right (mul x (App y z))
  --   parse "x y * z" term `shouldBe` Right (mul (App x y) z)
  --   parse "x y z" term `shouldBe` Right (App (App x y) z)

  it "☯ definition" $ do
    let definition' src ctx = fmap (\(x, a) -> (x, a ctx)) (parse src definition)
    definition' "x = 1" empty `shouldBe` Right ("x", Int 1)
