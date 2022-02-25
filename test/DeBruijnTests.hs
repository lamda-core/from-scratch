module DeBruijnTests where

import DeBruijn
import Test.Hspec

deBruijnTests :: SpecWith ()
deBruijnTests = describe "--== DeBruijn ==--" $ do
  let (i1, i2, i3) = (Int 1, Int 2, Int 3)
  describe "☯ show" $ do
    it "☯ basic expressions" $ do
      show Any `shouldBe` "_"
      show Tup `shouldBe` "()"
      show Add `shouldBe` "(+)"
      show Sub `shouldBe` "(-)"
      show Mul `shouldBe` "(*)"
      show IntT `shouldBe` "%Int"
      show (Int 1) `shouldBe` "1"
      show (Var 0) `shouldBe` "$0"
      show (For "x" $ Var 0) `shouldBe` "@x. x"

    it "☯ operator precedence" $ do
      show (Or i1 (Or i2 i3)) `shouldBe` "1 | 2 | 3"
      show (Or (Or i1 i2) i3) `shouldBe` "(1 | 2) | 3"
      show (Or i1 (Ann i2 i3)) `shouldBe` "1 | 2 : 3"
      show (Or (Ann i1 i2) i3) `shouldBe` "1 : 2 | 3"
      show (Ann i1 (Ann i2 i3)) `shouldBe` "1 : 2 : 3"
      show (Ann (Ann i1 i2) i3) `shouldBe` "(1 : 2) : 3"
      show (Ann i1 (Lam i2 i3)) `shouldBe` "1 : 2 -> 3"
      show (Ann (Lam i1 i2) i3) `shouldBe` "1 -> 2 : 3"
      show (Lam i1 (Lam i2 i3)) `shouldBe` "1 -> 2 -> 3"
      show (Lam (Lam i1 i2) i3) `shouldBe` "(1 -> 2) -> 3"
      show (Lam i1 (add i2 i3)) `shouldBe` "1 -> 2 + 3"
      show (Lam i1 (sub i2 i3)) `shouldBe` "1 -> 2 - 3"
      show (Lam (add i1 i2) i3) `shouldBe` "1 + 2 -> 3"
      show (Lam (sub i1 i2) i3) `shouldBe` "1 - 2 -> 3"
      show (add (add i1 i2) i3) `shouldBe` "1 + 2 + 3"
      show (add i1 (add i2 i3)) `shouldBe` "1 + (2 + 3)"
      show (add i1 (sub i2 i3)) `shouldBe` "1 + (2 - 3)"
      show (add (sub i1 i2) i3) `shouldBe` "1 - 2 + 3"
      show (add i1 (mul i2 i3)) `shouldBe` "1 + 2 * 3"
      show (add (mul i1 i2) i3) `shouldBe` "1 * 2 + 3"
      show (sub (add i1 i2) i3) `shouldBe` "1 + 2 - 3"
      show (sub i1 (add i2 i3)) `shouldBe` "1 - (2 + 3)"
      show (sub i1 (sub i2 i3)) `shouldBe` "1 - (2 - 3)"
      show (sub (sub i1 i2) i3) `shouldBe` "1 - 2 - 3"
      show (sub i1 (mul i2 i3)) `shouldBe` "1 - 2 * 3"
      show (sub (mul i1 i2) i3) `shouldBe` "1 * 2 - 3"
      show (mul (mul i1 i2) i3) `shouldBe` "1 * 2 * 3"
      show (mul i1 (mul i2 i3)) `shouldBe` "1 * (2 * 3)"
      show (mul i1 (App i2 i3)) `shouldBe` "1 * 2 3"
      show (mul (App i1 i2) i3) `shouldBe` "1 2 * 3"
      show (App (App i1 i2) i3) `shouldBe` "1 2 3"
      show (App i1 (App i2 i3)) `shouldBe` "1 (2 3)"

    it "☯ syntax sugar" $ do
      show (For "x" $ For "y" $ For "z" $ Var 2) `shouldBe` "@x y z. x"

  describe "☯ parse" $ do
    it "☯ terms" $ do
      parse "_" [] `shouldBe` Right Any
      parse "()" [] `shouldBe` Right Tup
      parse "( )" [] `shouldBe` Right Tup
      parse "(+)" [] `shouldBe` Right Add
      parse "( + )" [] `shouldBe` Right Add
      parse "(-)" [] `shouldBe` Right Sub
      parse "( - )" [] `shouldBe` Right Sub
      parse "(*)" [] `shouldBe` Right Mul
      parse "( * )" [] `shouldBe` Right Mul
      parse "%Int" [] `shouldBe` Right IntT
      parse "1" [] `shouldBe` Right i1
      parse "x" [] `shouldBe` Left (UndefinedName "x")
      parse "x" ["x"] `shouldBe` Right (Var 0)
      parse "x" ["z", "y", "x"] `shouldBe` Right (Var 2)

    it "☯ unary operators" $ do
      parse "@x.x" [] `shouldBe` Right (For "x" $ Var 0)
      parse "@ x . x" [] `shouldBe` Right (For "x" $ Var 0)
      parse "@x. @y. x" [] `shouldBe` Right (For "x" $ For "y" $ Var 1)
      parse "(_)" [] `shouldBe` Right Any
      parse "( _ )" [] `shouldBe` Right Any

    it "☯ binary operators" $ do
      parse "1|2" [] `shouldBe` Right (Or i1 i2)
      parse "1 | 2" [] `shouldBe` Right (Or i1 i2)
      parse "1:2" [] `shouldBe` Right (Ann i1 i2)
      parse "1 : 2" [] `shouldBe` Right (Ann i1 i2)
      parse "1->2" [] `shouldBe` Right (Lam i1 i2)
      parse "1 -> 2" [] `shouldBe` Right (Lam i1 i2)
      parse "1+2" [] `shouldBe` Right (add i1 i2)
      parse "1 + 2" [] `shouldBe` Right (add i1 i2)
      parse "1-2" [] `shouldBe` Right (sub i1 i2)
      parse "1 - 2" [] `shouldBe` Right (sub i1 i2)
      parse "1*2" [] `shouldBe` Right (mul i1 i2)
      parse "1 * 2" [] `shouldBe` Right (mul i1 i2)
      parse "1 2" [] `shouldBe` Right (App i1 i2)
      parse "1  2" [] `shouldBe` Right (App i1 i2)

    it "☯ operator precedence" $ do
      parse "1 | 2 | 3" [] `shouldBe` Right (Or i1 (Or i2 i3))
      parse "(1 | 2) | 3" [] `shouldBe` Right (Or (Or i1 i2) i3)
      parse "1 | 2 : 3" [] `shouldBe` Right (Or i1 (Ann i2 i3))
      parse "1 : 2 | 3" [] `shouldBe` Right (Or (Ann i1 i2) i3)
      parse "1 : 2 : 3" [] `shouldBe` Right (Ann i1 (Ann i2 i3))
      parse "(1 : 2) : 3" [] `shouldBe` Right (Ann (Ann i1 i2) i3)
      parse "1 : 2 -> 3" [] `shouldBe` Right (Ann i1 (Lam i2 i3))
      parse "1 -> 2 : 3" [] `shouldBe` Right (Ann (Lam i1 i2) i3)
      parse "1 -> 2 -> 3" [] `shouldBe` Right (Lam i1 (Lam i2 i3))
      parse "(1 -> 2) -> 3" [] `shouldBe` Right (Lam (Lam i1 i2) i3)
      parse "1 -> 2 + 3" [] `shouldBe` Right (Lam i1 (add i2 i3))
      parse "1 -> 2 - 3" [] `shouldBe` Right (Lam i1 (sub i2 i3))
      parse "1 + 2 -> 3" [] `shouldBe` Right (Lam (add i1 i2) i3)
      parse "1 - 2 -> 3" [] `shouldBe` Right (Lam (sub i1 i2) i3)
      parse "1 + 2 + 3" [] `shouldBe` Right (add (add i1 i2) i3)
      parse "1 + (2 + 3)" [] `shouldBe` Right (add i1 (add i2 i3))
      parse "1 + (2 - 3)" [] `shouldBe` Right (add i1 (sub i2 i3))
      parse "1 - 2 + 3" [] `shouldBe` Right (add (sub i1 i2) i3)
      parse "1 + 2 * 3" [] `shouldBe` Right (add i1 (mul i2 i3))
      parse "1 * 2 + 3" [] `shouldBe` Right (add (mul i1 i2) i3)
      parse "1 + 2 - 3" [] `shouldBe` Right (sub (add i1 i2) i3)
      parse "1 - (2 + 3)" [] `shouldBe` Right (sub i1 (add i2 i3))
      parse "1 - (2 - 3)" [] `shouldBe` Right (sub i1 (sub i2 i3))
      parse "1 - 2 - 3" [] `shouldBe` Right (sub (sub i1 i2) i3)
      parse "1 - 2 * 3" [] `shouldBe` Right (sub i1 (mul i2 i3))
      parse "1 * 2 - 3" [] `shouldBe` Right (sub (mul i1 i2) i3)
      parse "1 * 2 * 3" [] `shouldBe` Right (mul (mul i1 i2) i3)
      parse "1 * (2 * 3)" [] `shouldBe` Right (mul i1 (mul i2 i3))
      parse "1 * 2 3" [] `shouldBe` Right (mul i1 (App i2 i3))
      parse "1 2 * 3" [] `shouldBe` Right (mul (App i1 i2) i3)
      parse "1 2 3" [] `shouldBe` Right (App (App i1 i2) i3)
      parse "1 (2 3)" [] `shouldBe` Right (App i1 (App i2 i3))

    it "☯ syntax sugar" $ do
      -- TODO: (1, 2)  ==>  () 1 2
      -- TODO: \(x, x) -> x  ==>  @x. (x, x) -> x
      -- TODO: (+ 1)  ==>  @x. x -> x + 1
      -- TODO: (1 +)  ==>  (+) 1
      parse "@x y z. x" [] `shouldBe` Right (For "x" $ For "y" $ For "z" $ Var 2)

  describe "☯ reduction rules" $ do
    it "☯ eval" $ do
      let eval' :: String -> Env -> Either Error Expr
          eval' a env = do
            a <- parse a (fmap fst env)
            Right (eval a env)
      eval' "_" [] `shouldBe` Right Any
      eval' "()" [] `shouldBe` Right Tup
      eval' "(+)" [] `shouldBe` Right Add
      eval' "(-)" [] `shouldBe` Right Sub
      eval' "(*)" [] `shouldBe` Right Mul
      eval' "%Int" [] `shouldBe` Right IntT
      eval' "1" [] `shouldBe` Right i1
      eval' "x" [("x", i1)] `shouldBe` Right i1
      eval' "@x. y" [("y", i1)] `shouldBe` Right i1
      eval' "x | 2" [("x", i1)] `shouldBe` Right i1
      eval' "x : %Int" [("x", i1)] `shouldBe` Right i1
      eval' "x -> y" [("x", Tup), ("y", i1)] `shouldBe` Right (Lam Tup i1)
      eval' "x y" [("x", Tup), ("y", i1)] `shouldBe` Right (App Tup i1)
      eval' "(0 -> 1) 0" [] `shouldBe` Right i1
      eval' "(0 -> 1 | _ -> 2) 0" [] `shouldBe` Right i1
      eval' "(0 -> 1 | _ -> 2) 1" [] `shouldBe` Right i2
      eval' "x + x" [("x", i1)] `shouldBe` Right (Int 2)
      eval' "x - x" [("x", i1)] `shouldBe` Right (Int 0)
      eval' "x * x" [("x", i1)] `shouldBe` Right (Int 1)

    -- it "☯ typeOf" $ do
    -- Any
    -- Tup
    -- Add
    -- Sub
    -- Mul
    -- IntT
    -- Int Int
    -- Var Int
    -- For String Expr
    -- Or Expr Expr
    -- Ann Expr Typ
    -- Lam Pattern Expr
    -- App Expr Expr

    it "☯ match" $ do
      let match' :: String -> String -> Env -> Either Expr Env
          match' p a env =
            let vars = fmap fst env
             in case (parse p vars, parse a vars) of
                  (Right p, Right a) -> match p a env
                  _ -> Left Any
      match' "_" "()" [] `shouldBe` Right []
      match' "()" "_" [] `shouldBe` Left Any
      match' "()" "()" [] `shouldBe` Right []
      match' "(+)" "(+)" [] `shouldBe` Right []
      match' "(-)" "(-)" [] `shouldBe` Right []
      match' "(*)" "(*)" [] `shouldBe` Right []
      match' "%Int" "%Int" [] `shouldBe` Right []
      match' "1" "1" [] `shouldBe` Right []
      match' "1" "2" [] `shouldBe` Left i2
      match' "1" "1 + 1" [] `shouldBe` Left i2
      match' "2" "1 + 1" [] `shouldBe` Right []
      match' "x" "1 + 1" [("x", Any)] `shouldBe` Right [("x", add i1 i1)]
      match' "x" "1" [("x", i1)] `shouldBe` Right [("x", i1)]
      match' "x" "2" [("x", i1)] `shouldBe` Left i2
      match' "x" "1" [("x", Any)] `shouldBe` Right [("x", i1)]
      match' "x" "1" [("y", Any), ("x", Any)] `shouldBe` Right [("y", Any), ("x", i1)]
      match' "x" "y" [("x", Any), ("y", i1)] `shouldBe` Right [("x", Var 1), ("y", i1)]
      match' "@x. x" "1" [] `shouldBe` Right [("x", i1)]
      match' "x | 2" "1" [("x", Any)] `shouldBe` Right [("x", i1)]
      match' "2 | x" "1" [("x", Any)] `shouldBe` Right [("x", i1)]
      match' "x : %Int" "1 : %Int" [("x", Any)] `shouldBe` Right [("x", i1)]
      match' "1 : x" "1 : %Int" [("x", Any)] `shouldBe` Right [("x", IntT)]
      match' "x : %Int" "1" [("x", Any)] `shouldBe` Right [("x", i1)]
      match' "1 : x" "1" [("x", Any)] `shouldBe` Right [("x", IntT)]
      match' "x -> 2" "1 -> 2" [("x", Any)] `shouldBe` Right [("x", i1)]
      match' "1 -> x" "1 -> 2" [("x", Any)] `shouldBe` Right [("x", i2)]
      match' "1 -> x" "(y -> 1 -> y) 2" [("x", Any), ("y", Any)] `shouldBe` Right [("x", Var 1), ("y", i2)]
      match' "x 1" "() 1" [("x", Any)] `shouldBe` Right [("x", Tup)]
      match' "() x" "() 1" [("x", Any)] `shouldBe` Right [("x", i1)]
      match' "() x" "(y -> () y) 2" [("x", Any), ("y", Any)] `shouldBe` Right [("x", Var 1), ("y", i2)]
