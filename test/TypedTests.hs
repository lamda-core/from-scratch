module TypedTests where

import Test.Hspec
import Typed

typedTests :: SpecWith ()
typedTests = describe "--== Typed ==--" $ do
  describe "☯ parse" $ do
    let (x, y, z) = (Var "x", Var "y", Var "z")
    it "☯ terms" $ do
      parse "_" `shouldBe` Right Any
      parse "()" `shouldBe` Right Tup
      parse "%Int" `shouldBe` Right IntT
      parse "42" `shouldBe` Right (Int 42)
      parse "x" `shouldBe` Right x
      parse "(+)" `shouldBe` Right Add
      parse "(-)" `shouldBe` Right Sub
      parse "(*)" `shouldBe` Right Mul

    it "☯ unary operators" $ do
      parse "@x.y" `shouldBe` Right (For "x" y)
      parse "@ x . y" `shouldBe` Right (For "x" y)
      parse "(x)" `shouldBe` Right x
      parse "( x )" `shouldBe` Right x

    it "☯ binary operators" $ do
      parse "x:y" `shouldBe` Right (Ann x y)
      parse "x : y" `shouldBe` Right (Ann x y)
      parse "x->y" `shouldBe` Right (Lam x y)
      parse "x -> y" `shouldBe` Right (Lam x y)
      parse "x|y" `shouldBe` Right (Or x y)
      parse "x | y" `shouldBe` Right (Or x y)
      parse "x+y" `shouldBe` Right (add x y)
      parse "x + y" `shouldBe` Right (add x y)
      parse "x-y" `shouldBe` Right (sub x y)
      parse "x - y" `shouldBe` Right (sub x y)
      parse "x*y" `shouldBe` Right (mul x y)
      parse "x * y" `shouldBe` Right (mul x y)
      parse "x y" `shouldBe` Right (App x y)
      parse "x  y" `shouldBe` Right (App x y)

    it "☯ operator precedence" $ do
      parse "x | y | z" `shouldBe` Right (Or (Or x y) z)
      parse "x | y : z" `shouldBe` Right (Or x (Ann y z))
      parse "x | y -> z" `shouldBe` Right (Or x (Lam y z))
      parse "x | y + z" `shouldBe` Right (Or x (add y z))
      parse "x | y - z" `shouldBe` Right (Or x (sub y z))
      parse "x | y * z" `shouldBe` Right (Or x (mul y z))
      parse "x | y z" `shouldBe` Right (Or x (App y z))
      parse "x : y | z" `shouldBe` Right (Or (Ann x y) z)
      parse "x : y : z" `shouldBe` Right (Ann x (Ann y z))
      parse "x : y -> z" `shouldBe` Right (Ann x (Lam y z))
      parse "x : y + z" `shouldBe` Right (Ann x (add y z))
      parse "x : y - z" `shouldBe` Right (Ann x (sub y z))
      parse "x : y * z" `shouldBe` Right (Ann x (mul y z))
      parse "x : y z" `shouldBe` Right (Ann x (App y z))
      parse "x -> y | z" `shouldBe` Right (Or (Lam x y) z)
      parse "x -> y : z" `shouldBe` Right (Ann (Lam x y) z)
      parse "x -> y -> z" `shouldBe` Right (Lam x (Lam y z))
      parse "x -> y + z" `shouldBe` Right (Lam x (add y z))
      parse "x -> y - z" `shouldBe` Right (Lam x (sub y z))
      parse "x -> y * z" `shouldBe` Right (Lam x (mul y z))
      parse "x -> y z" `shouldBe` Right (Lam x (App y z))
      parse "x + y | z" `shouldBe` Right (Or (add x y) z)
      parse "x + y : z" `shouldBe` Right (Ann (add x y) z)
      parse "x + y -> z" `shouldBe` Right (Lam (add x y) z)
      parse "x + y + z" `shouldBe` Right (add (add x y) z)
      parse "x + y - z" `shouldBe` Right (sub (add x y) z)
      parse "x + y * z" `shouldBe` Right (add x (mul y z))
      parse "x + y z" `shouldBe` Right (add x (App y z))
      parse "x - y | z" `shouldBe` Right (Or (sub x y) z)
      parse "x - y : z" `shouldBe` Right (Ann (sub x y) z)
      parse "x - y -> z" `shouldBe` Right (Lam (sub x y) z)
      parse "x - y + z" `shouldBe` Right (add (sub x y) z)
      parse "x - y - z" `shouldBe` Right (sub (sub x y) z)
      parse "x - y * z" `shouldBe` Right (sub x (mul y z))
      parse "x - y z" `shouldBe` Right (sub x (App y z))
      parse "x * y | z" `shouldBe` Right (Or (mul x y) z)
      parse "x * y : z" `shouldBe` Right (Ann (mul x y) z)
      parse "x * y -> z" `shouldBe` Right (Lam (mul x y) z)
      parse "x * y + z" `shouldBe` Right (add (mul x y) z)
      parse "x * y - z" `shouldBe` Right (sub (mul x y) z)
      parse "x * y * z" `shouldBe` Right (mul (mul x y) z)
      parse "x * y z" `shouldBe` Right (mul x (App y z))
      parse "x y | z" `shouldBe` Right (Or (App x y) z)
      parse "x y : z" `shouldBe` Right (Ann (App x y) z)
      parse "x y -> z" `shouldBe` Right (Lam (App x y) z)
      parse "x y + z" `shouldBe` Right (add (App x y) z)
      parse "x y - z" `shouldBe` Right (sub (App x y) z)
      parse "x y * z" `shouldBe` Right (mul (App x y) z)
      parse "x y z" `shouldBe` Right (App (App x y) z)
      parse "x (y z)" `shouldBe` Right (App x (App y z))

    it "☯ syntax sugar" $ do
      parse "@x y z. ()" `shouldBe` Right (For "x" (For "y" (For "z" Tup)))

  it "☯ occurs" $ do
    let occurs' x expr = fmap (occurs x) (parse expr)
    occurs' "x" "_" `shouldBe` Right False
    occurs' "x" "()" `shouldBe` Right False
    occurs' "x" "%Int" `shouldBe` Right False
    occurs' "x" "1" `shouldBe` Right False
    occurs' "x" "x" `shouldBe` Right True
    occurs' "x" "y" `shouldBe` Right False
    occurs' "x" "@x. x" `shouldBe` Right False
    occurs' "x" "@y. x" `shouldBe` Right True
    occurs' "x" "x | y" `shouldBe` Right True
    occurs' "x" "y | x" `shouldBe` Right True
    occurs' "x" "z | z" `shouldBe` Right False
    occurs' "x" "x : y" `shouldBe` Right True
    occurs' "x" "y : x" `shouldBe` Right True
    occurs' "x" "z : z" `shouldBe` Right False
    occurs' "x" "x -> y" `shouldBe` Right True
    occurs' "x" "y -> x" `shouldBe` Right True
    occurs' "x" "z -> z" `shouldBe` Right False
    occurs' "x" "x y" `shouldBe` Right True
    occurs' "x" "y x" `shouldBe` Right True
    occurs' "x" "z z" `shouldBe` Right False

  it "☯ match" $ do
    let match' :: String -> String -> Env -> Either Error Env
        match' p a env = do
          p <- parse p
          a <- parse a
          match p a env
    match' "()" "_" empty `shouldBe` Left (CannotMatch Tup Any)
    match' "_" "()" empty `shouldBe` Right empty
    match' "()" "()" empty `shouldBe` Right empty
    match' "()" "()" empty `shouldBe` Right empty
    match' "%Int" "%Int" empty `shouldBe` Right empty
    match' "1" "1" empty `shouldBe` Right empty
    match' "1" "2" empty `shouldBe` Left (CannotMatch (Int 1) (Int 2))
    match' "x" "()" empty `shouldBe` Left (UndefinedName "x")
    match' "x" "()" (fromList [("x", IntT)]) `shouldBe` Left (CannotMatch IntT Tup)
    match' "x" "()" (fromList [("x", Var "x")]) `shouldBe` Right (fromList [("x", Tup)])
    match' "()" "x" empty `shouldBe` Left (UndefinedName "x")
    match' "()" "x" (fromList [("x", Tup)]) `shouldBe` Right (fromList [("x", Tup)])
    match' "@x. x" "()" empty `shouldBe` Right (fromList [("x", Tup)])
    match' "@x. x | 2" "1" empty `shouldBe` Right (fromList [("x", Int 1)])
    match' "@x. 2 | x" "1" empty `shouldBe` Right (fromList [("x", Int 1)])
    -- TODO: Ann
    match' "@x y. x -> y" "1 -> 2" empty `shouldBe` Right (fromList [("x", Int 1), ("y", Int 2)])
    match' "@x y. x y" "z 1" (fromList [("z", Var "z")]) `shouldBe` Right (fromList [("x", Var "z"), ("y", Int 1), ("z", Var "z")])
    match' "(+)" "(+)" empty `shouldBe` Right empty
    match' "(-)" "(-)" empty `shouldBe` Right empty
    match' "(*)" "(*)" empty `shouldBe` Right empty

  -- it "☯ eval" $ do
  --   let (i0, i1, i2, i3, x) = (Int 0, Int 1, Int 2, Int 3, Var "x")
  --   eval (add i1 i1) empty `shouldBe` i2
  --   eval (sub i1 i1) empty `shouldBe` i0
  --   eval (mul i1 i1) empty `shouldBe` i1
  --   eval x empty `shouldBe` x
  --   eval x (fromList [("x", i1)]) `shouldBe` i1
  --   eval (add x x) (fromList [("x", i1)]) `shouldBe` i2
  --   -- eval (Or []) empty `shouldBe` Or []
  --   -- eval (Or [add i1 i1, add i2 i2]) empty `shouldBe` i2
  --   eval (Or (add i1 i1) (add i2 i2)) empty `shouldBe` i2
  --   eval (Ann (add i1 i1) IntT) empty `shouldBe` i2
  --   eval (App x (add i1 i1)) (fromList [("x", Tup)]) `shouldBe` App Tup i2
  --   eval (App (Ann x (Lam Tup IntT)) Tup) empty `shouldBe` App x Tup
  --   eval (App (Lam i1 i2) i3) empty `shouldBe` App Any i3
  --   eval (App (Lam i1 i2) x) (fromList [("x", i1)]) `shouldBe` i2
  --   eval (App (Lam (For "x" x) (add x x)) i1) empty `shouldBe` i2
  --   -- eval (App (Or []) i1) empty `shouldBe` App (Or []) i1
  --   -- eval (App (Or [Lam i1 i2, Lam Any i3]) i1) empty `shouldBe` i2
  --   -- eval (App (Or [Lam i1 i2, Lam Any i3]) i0) empty `shouldBe` i3
  --   eval (App (Or (Lam i1 i2) (Lam Any i3)) i1) empty `shouldBe` i2
  --   eval (App (Or (Lam i1 i2) (Lam Any i3)) i0) empty `shouldBe` i3
  --   eval (App (App x (add i1 i1)) (add i1 i2)) (fromList [("x", Tup)]) `shouldBe` App (App Tup i2) i3

  -- it "☯ intToName" $ do
  --   intToName 0 `shouldBe` ""
  --   intToName 1 `shouldBe` "a"
  --   intToName 26 `shouldBe` "z"
  --   intToName 27 `shouldBe` "aa"
  --   intToName 28 `shouldBe` "ba"

  -- it "☯ newVar" $ do
  --   newVar empty `shouldBe` ("a", (fromList [("a", Var "a")]) {seed = 2})
  --   newVar (fromList [("a", IntT)]) `shouldBe` ("b", (fromList [("a", IntT), ("b", Var "b")]) {seed = 3})

  -- it "☯ rename" $ do
  --   let rename' x y a env = fst (rename x y a env)
  --   rename' "x" "y" (Var "x") empty `shouldBe` Var "y"
  --   rename' "x" "y" (Ann (Var "x") (Var "x")) empty `shouldBe` Ann (Var "y") (Var "y")
  --   rename' "x" "y" (For "x" $ Ann (Var "x") (Var "y")) empty `shouldBe` For "x" (Ann (Var "x") (Var "y"))
  --   rename' "x" "y" (For "y" $ Ann (Var "x") (Var "y")) empty `shouldBe` For "a" (Ann (Var "y") (Var "a"))
  --   rename' "x" "y" (Lam (Var "x") (Var "x")) empty `shouldBe` Lam (Var "y") (Var "y")
  --   rename' "x" "y" (App (Var "x") (Var "x")) empty `shouldBe` App (Var "y") (Var "y")

  -- it "☯ instantiate" $ do
  --   instantiate Any empty `shouldBe` (Any, empty)
  --   instantiate (For "x" (Var "x")) empty `shouldBe` (Var "x", fromList [("x", Var "x")])
  --   instantiate (Ann (For "x" $ Var "x") (For "x" $ Var "x")) empty `shouldBe` (Ann (Var "x") (Var "a"), (fromList [("x", Var "x"), ("a", Var "a")]) {seed = 2})
  --   instantiate (Lam (For "x" $ Var "x") (For "x" $ Var "x")) empty `shouldBe` (Lam (Var "x") (Var "a"), (fromList [("x", Var "x"), ("a", Var "a")]) {seed = 2})
  --   instantiate (Lam (For "x" $ Var "x") (For "x" $ Var "x")) empty `shouldBe` (Lam (Var "x") (Var "a"), (fromList [("x", Var "x"), ("a", Var "a")]) {seed = 2})
  --   instantiate (App (For "x" $ Var "x") (For "x" $ Var "x")) empty `shouldBe` (App (Var "x") (Var "a"), (fromList [("x", Var "x"), ("a", Var "a")]) {seed = 2})

  it "☯ unify" $ do
    let unify' :: String -> String -> Env -> Either Error (Expr, Env)
        unify' a b env = do
          a <- parse a
          b <- parse b
          unify a b env
    unify' "_" "()" empty `shouldBe` Right (Tup, empty)
    unify' "()" "_" empty `shouldBe` Right (Tup, empty)
    unify' "()" "()" empty `shouldBe` Right (Tup, empty)
    unify' "()" "%Int" empty `shouldBe` Left (CannotUnify Tup IntT)
    unify' "()" "()" empty `shouldBe` Right (Tup, empty)
    unify' "x" "()" empty `shouldBe` Right (Tup, fromList [("x", Tup)]) -- TODO: error?
    unify' "()" "x" empty `shouldBe` Right (Tup, fromList [("x", Tup)]) -- TODO: error?
    unify' "x" "x : ()" empty `shouldBe` Left (CannotUnify (Var "x") (Ann (Var "x") Tup))
    unify' "x : ()" "x" empty `shouldBe` Left (CannotUnify (Ann (Var "x") Tup) (Var "x"))
    unify' "@x. x" "()" empty `shouldBe` Right (Tup, fromList [("x", Tup)])
    unify' "()" "@x. x" empty `shouldBe` Right (Tup, fromList [("x", Tup)])
    unify' "x : %Int" "1 : y" empty `shouldBe` Right (Ann (Int 1) IntT, fromList [("x", Int 1), ("y", IntT)])
    -- unify' "1 : x" "1" empty `shouldBe` Right (Ann (Int 1) IntT, fromList [("x", IntT)])
    unify' "x -> ()" "1 -> y" empty `shouldBe` Right (Lam (Int 1) Tup, fromList [("x", Int 1), ("y", Tup)])

-- it "☯ defineType" $ do
--   -- TODO: FIX THIS!
--   defineType "T" [] [] empty `shouldBe` fromList [("T", Typ [])]
--   defineType "T" [Var "a", Var "b"] [] empty `shouldBe` fromList [("T", Lam (Var "a") $ Lam (Var "b") $ Typ [])]
--   -- defineType "T" [Var "a", Var "b"] [] empty `shouldBe` fromList [("T", For "a" $ For "b" $ Lun (Var "a") $ Fun (Var "b") $ Typ [])]
--   defineType "T" [] [("A", Var "T"), ("B", Lam IntT (Var "T"))] empty
--     `shouldBe` fromList
--       [ ("T", Typ ["A", "B"]),
--         ("A", Var "T"),
--         ("B", Lam IntT $ Var "T")
--       ]

-- it "☯ typecheck" $ do
--   let typecheck' a env = fmap fst (typecheck a env)
--   typecheck' Any empty `shouldBe` Right Any
--   typecheck' Tup empty `shouldBe` Right Tup
--   typecheck' IntT empty `shouldBe` Right (Typ [])
--   typecheck' (Int 1) empty `shouldBe` Right IntT
--   typecheck' (Var "x") empty `shouldBe` Left (UndefinedName "x")
--   typecheck' (Var "x") (fromList [("x", Int 1)]) `shouldBe` Right IntT
--   typecheck' (Var "x") (fromList [("x", Var "x")]) `shouldBe` Right (Var "x")
--   typecheck' (Var "x") (fromList [("x", Ann (Var "x") IntT)]) `shouldBe` Right IntT
--   -- typecheck' (Or []) empty `shouldBe` Right Any
--   -- typecheck' (Or [Int 1, Int 2]) empty `shouldBe` Right IntT
--   -- typecheck' (Or [Tup, Int 1]) empty `shouldBe` Left (CannotUnify Tup IntT)
--   typecheck' (Or (Int 1) (Int 2)) empty `shouldBe` Right IntT
--   typecheck' (Or Tup (Int 1)) empty `shouldBe` Left (CannotUnify Tup IntT)
--   -- typecheck' (For "x" $ Var "x") empty `shouldBe` Right (For "x" $ Var "x")
--   typecheck' (For "x" $ Int 1) empty `shouldBe` Right IntT
--   typecheck' (Ann (Int 1) Tup) empty `shouldBe` Left (CannotUnify Tup IntT)
--   typecheck' (Ann (Int 1) IntT) empty `shouldBe` Right IntT
--   typecheck' (Typ ["A"]) empty `shouldBe` Right (Typ [])
--   typecheck' (Lam (Int 1) (Int 2)) empty `shouldBe` Right (Lam IntT IntT)
--   typecheck' (Lam (For "x" $ Var "x") (Var "x")) empty `shouldBe` Right (Lam (Var "x") (Var "x"))
--   typecheck' (App (Int 1) Tup) empty `shouldBe` Left (CannotUnify IntT (Lam Tup (Var "a")))
--   typecheck' (App (Lam Any (Int 1)) Tup) empty `shouldBe` Right IntT
--   typecheck' Add empty `shouldBe` Right (Lam (Var "a") $ Lam (Var "a") (Var "a"))
--   typecheck' Sub empty `shouldBe` Right (Lam (Var "a") $ Lam (Var "a") (Var "a"))
--   typecheck' Mul empty `shouldBe` Right (Lam (Var "a") $ Lam (Var "a") (Var "a"))

-- it "☯ alternatives" $ do
--   let env = defineType "Maybe" [Var "a"] [("Just", Fun (Var "a") (App (Ctr "Maybe") (Var "a"))), ("Nothing", App (Ctr "Maybe") (Var "a"))] empty
--   alternatives IntT env `shouldBe` Right [PAny]
--   alternatives (Typ []) env `shouldBe` Right []
--   alternatives (Typ ["Nothing"]) env `shouldBe` Right [PCtr "Nothing" []]
--   alternatives (Var "X") env `shouldBe` Left (UndefinedName "X")
--   alternatives (Var "Maybe") env `shouldBe` Right [PCtr "Just" [PAny], PCtr "Nothing" []]
--   alternatives (App (Var "Maybe") (Var "a")) env `shouldBe` Right [PCtr "Just" [PAny], PCtr "Nothing" []]
--   alternatives (Fun (Var "a") (App (Var "Maybe") (Var "a"))) env `shouldBe` Right [PCtr "Just" [PAny], PCtr "Nothing" []]

-- it "☯ specialize" $ do
--   let env = defineType "Maybe" [Var "a"] [("Just", Fun (Var "a") (App (Ctr "Maybe") (Var "a"))), ("Nothing", App (Ctr "Maybe") (Var "a"))] empty
--   specialize [] env `shouldBe` Right []
--   specialize [PInt 1] env `shouldBe` Right [PAny]
--   specialize [PCtr "Nothing" []] env `shouldBe` Right [PCtr "Just" [PAny], PCtr "Nothing" []]
--   specialize [PCtr "Just" []] env `shouldBe` Right [PCtr "Just" [PAny], PCtr "Nothing" []]

-- it "☯ specialize" $ do
--   let env = defineType "T" [] [("A", Var "T"), ("B", Fun IntT $ Var "T")] Map.empty
--   specialize PAny env `shouldBe` Right [PAny]
--   specialize (PInt 1) env `shouldBe` Right [PAny]
--   specialize (PVar "x") env `shouldBe` Right [PAny]
--   specialize (PCtr "X" []) env `shouldBe` Left (UndefinedName "X")
--   specialize (PCtr "A" []) env `shouldBe` Right [PCtr "A" [], PCtr "B" [PAny]]
--   specialize (PCtr "B" []) env `shouldBe` Left (NumArgsMismatch {ctr = "B", expected = 1, got = 0})
--   specialize (PCtr "B" [PCtr "A" []]) env `shouldBe` Right [PCtr "A" [], PCtr "B" [PCtr "A" []], PCtr "B" [PCtr "B" [PAny]]]
--   specialize (PTup []) env `shouldBe` Right [PTup []]
--   specialize (PTup [PCtr "A" [], PCtr "A" []]) env
--     `shouldBe` Right
--       [ PTup [PCtr "A" [], PCtr "A" []],
--         PTup [PCtr "A" [], PCtr "B" [PAny]],
--         PTup [PCtr "B" [PAny], PCtr "A" []],
--         PTup [PCtr "B" [PAny], PCtr "B" [PAny]]
--       ]
