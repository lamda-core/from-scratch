module TypedTests where

import Parser (parse)
import Test.Hspec
import Typed

typedTests :: SpecWith ()
typedTests = describe "--== Typed ==--" $ do
  describe "☯ parser" $ do
    let (x, y, z) = (Var "x", Var "y", Var "z")
    it "☯ terms" $ do
      parse "_" expression `shouldBe` Right Any
      parse "()" expression `shouldBe` Right Tup
      parse "$Int" expression `shouldBe` Right IntT
      parse "42" expression `shouldBe` Right (Int 42)
      parse "x" expression `shouldBe` Right x
      parse "$Type" expression `shouldBe` Right (Typ [])
      parse "$Type!True!False" expression `shouldBe` Right (Typ ["True", "False"])
      parse "$Type ! True ! False" expression `shouldBe` Right (Typ ["True", "False"])
      parse "(+)" expression `shouldBe` Right Add
      parse "(-)" expression `shouldBe` Right Sub
      parse "(*)" expression `shouldBe` Right Mul

    it "☯ unary operators" $ do
      parse "@x.y" expression `shouldBe` Right (For "x" y)
      parse "@ x . y" expression `shouldBe` Right (For "x" y)
      parse "(x)" expression `shouldBe` Right x
      parse "( x )" expression `shouldBe` Right x

    it "☯ binary operators" $ do
      parse "x:y" expression `shouldBe` Right (Ann x y)
      parse "x : y" expression `shouldBe` Right (Ann x y)
      parse "x->y" expression `shouldBe` Right (Lam x y)
      parse "x -> y" expression `shouldBe` Right (Lam x y)
      parse "x|y" expression `shouldBe` Right (Or x y)
      parse "x | y" expression `shouldBe` Right (Or x y)
      parse "x+y" expression `shouldBe` Right (add x y)
      parse "x + y" expression `shouldBe` Right (add x y)
      parse "x-y" expression `shouldBe` Right (sub x y)
      parse "x - y" expression `shouldBe` Right (sub x y)
      parse "x*y" expression `shouldBe` Right (mul x y)
      parse "x * y" expression `shouldBe` Right (mul x y)
      parse "x y" expression `shouldBe` Right (App x y)
      parse "x  y" expression `shouldBe` Right (App x y)

    -- TODO: add | for precedence
    it "☯ operator precedence" $ do
      parse "x | y | z" expression `shouldBe` Right (Or (Or x y) z)
      parse "x | y : z" expression `shouldBe` Right (Or x (Ann y z))
      parse "x | y -> z" expression `shouldBe` Right (Or x (Lam y z))
      parse "x | y + z" expression `shouldBe` Right (Or x (add y z))
      parse "x | y - z" expression `shouldBe` Right (Or x (sub y z))
      parse "x | y * z" expression `shouldBe` Right (Or x (mul y z))
      parse "x | y z" expression `shouldBe` Right (Or x (App y z))
      parse "x : y | z" expression `shouldBe` Right (Or (Ann x y) z)
      parse "x : y : z" expression `shouldBe` Right (Ann x (Ann y z))
      parse "x : y -> z" expression `shouldBe` Right (Ann x (Lam y z))
      parse "x : y + z" expression `shouldBe` Right (Ann x (add y z))
      parse "x : y - z" expression `shouldBe` Right (Ann x (sub y z))
      parse "x : y * z" expression `shouldBe` Right (Ann x (mul y z))
      parse "x : y z" expression `shouldBe` Right (Ann x (App y z))
      parse "x -> y | z" expression `shouldBe` Right (Or (Lam x y) z)
      parse "x -> y : z" expression `shouldBe` Right (Ann (Lam x y) z)
      parse "x -> y -> z" expression `shouldBe` Right (Lam x (Lam y z))
      parse "x -> y + z" expression `shouldBe` Right (Lam x (add y z))
      parse "x -> y - z" expression `shouldBe` Right (Lam x (sub y z))
      parse "x -> y * z" expression `shouldBe` Right (Lam x (mul y z))
      parse "x -> y z" expression `shouldBe` Right (Lam x (App y z))
      parse "x + y | z" expression `shouldBe` Right (Or (add x y) z)
      parse "x + y : z" expression `shouldBe` Right (Ann (add x y) z)
      parse "x + y -> z" expression `shouldBe` Right (Lam (add x y) z)
      parse "x + y + z" expression `shouldBe` Right (add (add x y) z)
      parse "x + y - z" expression `shouldBe` Right (sub (add x y) z)
      parse "x + y * z" expression `shouldBe` Right (add x (mul y z))
      parse "x + y z" expression `shouldBe` Right (add x (App y z))
      parse "x - y | z" expression `shouldBe` Right (Or (sub x y) z)
      parse "x - y : z" expression `shouldBe` Right (Ann (sub x y) z)
      parse "x - y -> z" expression `shouldBe` Right (Lam (sub x y) z)
      parse "x - y + z" expression `shouldBe` Right (add (sub x y) z)
      parse "x - y - z" expression `shouldBe` Right (sub (sub x y) z)
      parse "x - y * z" expression `shouldBe` Right (sub x (mul y z))
      parse "x - y z" expression `shouldBe` Right (sub x (App y z))
      parse "x * y | z" expression `shouldBe` Right (Or (mul x y) z)
      parse "x * y : z" expression `shouldBe` Right (Ann (mul x y) z)
      parse "x * y -> z" expression `shouldBe` Right (Lam (mul x y) z)
      parse "x * y + z" expression `shouldBe` Right (add (mul x y) z)
      parse "x * y - z" expression `shouldBe` Right (sub (mul x y) z)
      parse "x * y * z" expression `shouldBe` Right (mul (mul x y) z)
      parse "x * y z" expression `shouldBe` Right (mul x (App y z))
      parse "x y | z" expression `shouldBe` Right (Or (App x y) z)
      parse "x y : z" expression `shouldBe` Right (Ann (App x y) z)
      parse "x y -> z" expression `shouldBe` Right (Lam (App x y) z)
      parse "x y + z" expression `shouldBe` Right (add (App x y) z)
      parse "x y - z" expression `shouldBe` Right (sub (App x y) z)
      parse "x y * z" expression `shouldBe` Right (mul (App x y) z)
      parse "x y z" expression `shouldBe` Right (App (App x y) z)
      parse "x (y z)" expression `shouldBe` Right (App x (App y z))

  it "☯ defineType" $ do
    -- TODO: FIX THIS!
    defineType "T" [] [] empty `shouldBe` fromList [("T", Typ [])]
    defineType "T" [Var "a", Var "b"] [] empty `shouldBe` fromList [("T", Lam (Var "a") $ Lam (Var "b") $ Typ [])]
    -- defineType "T" [Var "a", Var "b"] [] empty `shouldBe` fromList [("T", For "a" $ For "b" $ Lun (Var "a") $ Fun (Var "b") $ Typ [])]
    defineType "T" [] [("A", Var "T"), ("B", Lam IntT (Var "T"))] empty
      `shouldBe` fromList
        [ ("T", Typ ["A", "B"]),
          ("A", Var "T"),
          ("B", Lam IntT $ Var "T")
        ]

  it "☯ occurs" $ do
    "x" `occurs` Any `shouldBe` False
    "x" `occurs` Tup `shouldBe` False
    "x" `occurs` IntT `shouldBe` False
    "x" `occurs` Int 1 `shouldBe` False
    "x" `occurs` Var "x" `shouldBe` True
    "x" `occurs` Var "y" `shouldBe` False
    -- TODO: For
    -- TODO: Ann
    "x" `occurs` Typ ["x"] `shouldBe` False
    "x" `occurs` Lam (Var "y") (Var "y") `shouldBe` False
    "x" `occurs` Lam (Var "x") (Var "y") `shouldBe` True
    "x" `occurs` Lam (Var "y") (Var "x") `shouldBe` True
    -- TODO: Lam
    "x" `occurs` App (Var "y") (Var "y") `shouldBe` False
    "x" `occurs` App (Var "x") (Var "y") `shouldBe` True
    "x" `occurs` App (Var "y") (Var "x") `shouldBe` True

  it "☯ unify" $ do
    unify Any IntT empty `shouldBe` Right (IntT, empty)
    unify IntT Any empty `shouldBe` Right (IntT, empty)
    unify IntT Tup empty `shouldBe` Left (CannotUnify IntT Tup)
    unify IntT IntT empty `shouldBe` Right (IntT, empty)
    unify (Var "x") IntT empty `shouldBe` Right (IntT, fromList [("x", IntT)])
    unify IntT (Var "x") empty `shouldBe` Right (IntT, fromList [("x", IntT)])
    unify (Var "x") (Ann (Var "x") IntT) empty `shouldBe` Left (CannotUnify (Var "x") (Ann (Var "x") IntT))
    unify (For "x" $ Var "x") Tup empty `shouldBe` Right (Tup, fromList [("x", Tup)])
    unify Tup (For "x" $ Var "x") empty `shouldBe` Right (Tup, fromList [("x", Tup)])
    unify (Ann (Var "x") IntT) (Var "x") empty `shouldBe` Left (CannotUnify (Ann (Var "x") IntT) (Var "x"))
    unify (Ann (Var "x") IntT) (Ann (Int 1) IntT) empty `shouldBe` Right (Ann (Int 1) IntT, fromList [("x", Int 1)])
    unify (Ann (Int 1) (Var "x")) (Ann (Int 1) IntT) empty `shouldBe` Right (Ann (Int 1) IntT, fromList [("x", IntT)])
    -- unify (Ann (Int 1) (Var "x")) (Int 1) empty `shouldBe` Right (Ann (Int 1) IntT, fromList [("x", IntT)])
    unify (Lam (Var "x") IntT) (Lam Tup IntT) empty `shouldBe` Right (Lam Tup IntT, fromList [("x", Tup)])
    unify (Lam Tup (Var "x")) (Lam Tup IntT) empty `shouldBe` Right (Lam Tup IntT, fromList [("x", IntT)])

  it "☯ match" $ do
    match Any Tup empty `shouldBe` Just empty
    match Tup Tup empty `shouldBe` Just empty
    match Tup IntT empty `shouldBe` Nothing
    match (Var "x") Tup empty `shouldBe` Nothing
    match (Var "x") Tup (fromList [("x", Var "x")]) `shouldBe` Just (fromList [("x", Tup)])
    match (Var "x") Tup (fromList [("x", Any)]) `shouldBe` Just (fromList [("x", Any)])
    -- match (Or []) Tup empty `shouldBe` Nothing
    -- match (Or [IntT, Var "x"]) Tup (fromList [("x", Var "x")]) `shouldBe` Just (fromList [("x", Tup)])
    match (Or IntT (Var "x")) IntT (fromList [("x", Var "x")]) `shouldBe` Just (fromList [("x", Var "x")])
    match (Or IntT (Var "x")) Tup (fromList [("x", Var "x")]) `shouldBe` Just (fromList [("x", Tup)])
    match (For "x" (Var "y")) Tup (fromList [("y", Var "y")]) `shouldBe` Just (fromList [("x", Var "x"), ("y", Tup)])
    -- match (Ann (Var "x") (Var "y")) (Int 1) empty `shouldBe` Just (fromList [("x", Int 1), ("y", IntT)])
    match (Lam (Var "x") (Var "y")) (Lam Tup IntT) (fromList [("x", Var "x"), ("y", Var "y")]) `shouldBe` Just (fromList [("x", Tup), ("y", IntT)])
    match (Lam (Var "x") (Var "y")) (Var "z") (fromList [("x", Var "x"), ("y", Var "y"), ("z", Lam Tup IntT)]) `shouldBe` Just (fromList [("x", Tup), ("y", IntT), ("z", Lam Tup IntT)])
    match (App (Var "x") (Var "y")) (App Tup IntT) (fromList [("x", Var "x"), ("y", Var "y")]) `shouldBe` Just (fromList [("x", Tup), ("y", IntT)])
    match (App (Var "x") (Var "y")) (Var "z") (fromList [("x", Var "x"), ("y", Var "y"), ("z", App Tup IntT)]) `shouldBe` Just (fromList [("x", Tup), ("y", IntT), ("z", App Tup IntT)])

  it "☯ eval" $ do
    let (i0, i1, i2, i3, x) = (Int 0, Int 1, Int 2, Int 3, Var "x")
    eval (add i1 i1) empty `shouldBe` i2
    eval (sub i1 i1) empty `shouldBe` i0
    eval (mul i1 i1) empty `shouldBe` i1
    eval x empty `shouldBe` x
    eval x (fromList [("x", i1)]) `shouldBe` i1
    eval (add x x) (fromList [("x", i1)]) `shouldBe` i2
    -- eval (Or []) empty `shouldBe` Or []
    -- eval (Or [add i1 i1, add i2 i2]) empty `shouldBe` i2
    eval (Or (add i1 i1) (add i2 i2)) empty `shouldBe` i2
    eval (Ann (add i1 i1) IntT) empty `shouldBe` i2
    eval (App x (add i1 i1)) (fromList [("x", Tup)]) `shouldBe` App Tup i2
    eval (App (Ann x (Lam Tup IntT)) Tup) empty `shouldBe` App x Tup
    eval (App (Lam i1 i2) i3) empty `shouldBe` App Any i3
    eval (App (Lam i1 i2) x) (fromList [("x", i1)]) `shouldBe` i2
    eval (App (Lam (For "x" x) (add x x)) i1) empty `shouldBe` i2
    -- eval (App (Or []) i1) empty `shouldBe` App (Or []) i1
    -- eval (App (Or [Lam i1 i2, Lam Any i3]) i1) empty `shouldBe` i2
    -- eval (App (Or [Lam i1 i2, Lam Any i3]) i0) empty `shouldBe` i3
    eval (App (Or (Lam i1 i2) (Lam Any i3)) i1) empty `shouldBe` i2
    eval (App (Or (Lam i1 i2) (Lam Any i3)) i0) empty `shouldBe` i3
    eval (App (App x (add i1 i1)) (add i1 i2)) (fromList [("x", Tup)]) `shouldBe` App (App Tup i2) i3

  it "☯ intToName" $ do
    intToName 0 `shouldBe` ""
    intToName 1 `shouldBe` "a"
    intToName 26 `shouldBe` "z"
    intToName 27 `shouldBe` "aa"
    intToName 28 `shouldBe` "ba"

  it "☯ newVar" $ do
    newVar empty `shouldBe` ("a", (fromList [("a", Var "a")]) {seed = 2})
    newVar (fromList [("a", IntT)]) `shouldBe` ("b", (fromList [("a", IntT), ("b", Var "b")]) {seed = 3})

  it "☯ rename" $ do
    let rename' x y a env = fst (rename x y a env)
    rename' "x" "y" (Var "x") empty `shouldBe` Var "y"
    rename' "x" "y" (Ann (Var "x") (Var "x")) empty `shouldBe` Ann (Var "y") (Var "y")
    rename' "x" "y" (For "x" $ Ann (Var "x") (Var "y")) empty `shouldBe` For "x" (Ann (Var "x") (Var "y"))
    rename' "x" "y" (For "y" $ Ann (Var "x") (Var "y")) empty `shouldBe` For "a" (Ann (Var "y") (Var "a"))
    rename' "x" "y" (Lam (Var "x") (Var "x")) empty `shouldBe` Lam (Var "y") (Var "y")
    rename' "x" "y" (App (Var "x") (Var "x")) empty `shouldBe` App (Var "y") (Var "y")

  it "☯ instantiate" $ do
    instantiate Any empty `shouldBe` (Any, empty)
    instantiate (For "x" (Var "x")) empty `shouldBe` (Var "x", fromList [("x", Var "x")])
    instantiate (Ann (For "x" $ Var "x") (For "x" $ Var "x")) empty `shouldBe` (Ann (Var "x") (Var "a"), (fromList [("x", Var "x"), ("a", Var "a")]) {seed = 2})
    instantiate (Lam (For "x" $ Var "x") (For "x" $ Var "x")) empty `shouldBe` (Lam (Var "x") (Var "a"), (fromList [("x", Var "x"), ("a", Var "a")]) {seed = 2})
    instantiate (Lam (For "x" $ Var "x") (For "x" $ Var "x")) empty `shouldBe` (Lam (Var "x") (Var "a"), (fromList [("x", Var "x"), ("a", Var "a")]) {seed = 2})
    instantiate (App (For "x" $ Var "x") (For "x" $ Var "x")) empty `shouldBe` (App (Var "x") (Var "a"), (fromList [("x", Var "x"), ("a", Var "a")]) {seed = 2})

  it "☯ typecheck" $ do
    let typecheck' a env = fmap fst (typecheck a env)
    typecheck' Any empty `shouldBe` Right Any
    typecheck' Tup empty `shouldBe` Right Tup
    typecheck' IntT empty `shouldBe` Right (Typ [])
    typecheck' (Int 1) empty `shouldBe` Right IntT
    typecheck' (Var "x") empty `shouldBe` Left (UndefinedName "x")
    typecheck' (Var "x") (fromList [("x", Int 1)]) `shouldBe` Right IntT
    typecheck' (Var "x") (fromList [("x", Var "x")]) `shouldBe` Right (Var "x")
    typecheck' (Var "x") (fromList [("x", Ann (Var "x") IntT)]) `shouldBe` Right IntT
    -- typecheck' (Or []) empty `shouldBe` Right Any
    -- typecheck' (Or [Int 1, Int 2]) empty `shouldBe` Right IntT
    -- typecheck' (Or [Tup, Int 1]) empty `shouldBe` Left (CannotUnify Tup IntT)
    typecheck' (Or (Int 1) (Int 2)) empty `shouldBe` Right IntT
    typecheck' (Or Tup (Int 1)) empty `shouldBe` Left (CannotUnify Tup IntT)
    -- typecheck' (For "x" $ Var "x") empty `shouldBe` Right (For "x" $ Var "x")
    typecheck' (For "x" $ Int 1) empty `shouldBe` Right IntT
    typecheck' (Ann (Int 1) Tup) empty `shouldBe` Left (CannotUnify Tup IntT)
    typecheck' (Ann (Int 1) IntT) empty `shouldBe` Right IntT
    typecheck' (Typ ["A"]) empty `shouldBe` Right (Typ [])
    typecheck' (Lam (Int 1) (Int 2)) empty `shouldBe` Right (Lam IntT IntT)
    typecheck' (Lam (For "x" $ Var "x") (Var "x")) empty `shouldBe` Right (Lam (Var "x") (Var "x"))
    typecheck' (App (Int 1) Tup) empty `shouldBe` Left (CannotUnify IntT (Lam Tup (Var "a")))
    typecheck' (App (Lam Any (Int 1)) Tup) empty `shouldBe` Right IntT
    typecheck' Add empty `shouldBe` Right (Lam (Var "a") $ Lam (Var "a") (Var "a"))
    typecheck' Sub empty `shouldBe` Right (Lam (Var "a") $ Lam (Var "a") (Var "a"))
    typecheck' Mul empty `shouldBe` Right (Lam (Var "a") $ Lam (Var "a") (Var "a"))

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
