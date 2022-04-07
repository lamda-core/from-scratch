module CoreTests where

import Core
import Test.Hspec

coreTests :: SpecWith ()
coreTests = describe "--== Core ==--" $ do
  let (i0, i1, i2) = (Int 0, Int 1, Int 2)
  let (x, y, z) = (Var "x", Var "y", Var "z")
  let (a, b, c) = (Ctr "A", Ctr "B", Ctr "C")

  it "☯ occurs" $ do
    occurs "x" Err `shouldBe` False
    occurs "x" (Var "x") `shouldBe` True
    occurs "x" (Var "y") `shouldBe` False
    occurs "x" (Ctr "x") `shouldBe` False
    occurs "x" (Let [] x) `shouldBe` True
    occurs "x" (Let [("x", a)] x) `shouldBe` False
    occurs "x" (Let [("y", a)] x) `shouldBe` True
    occurs "x" (Lam x x) `shouldBe` False
    occurs "x" (Lam y y) `shouldBe` False
    occurs "x" (Lam y x) `shouldBe` True
    occurs "x" (Or y y) `shouldBe` False
    occurs "x" (Or x y) `shouldBe` True
    occurs "x" (Or y x) `shouldBe` True
    occurs "x" (App y y) `shouldBe` False
    occurs "x" (App x y) `shouldBe` True
    occurs "x" (App y x) `shouldBe` True

  -- TODO: organize by rules
  it "☯ constant" $ do
    reduce Err `shouldBe` Err
    reduce i0 `shouldBe` i0
    reduce x `shouldBe` x
    reduce a `shouldBe` a
  it "☯ variable lookup" $ do
    reduce (Let [("x", a)] x) `shouldBe` a
    reduce (Let [("x", a)] y) `shouldBe` Err
  it "☯ environment dropping" $ do
    reduce (Let [("x", a)] Err) `shouldBe` Err
    reduce (Let [("x", a)] i0) `shouldBe` i0
  it "☯ environment extension" $ do
    reduce (Let [("x", y)] $ Let [("y", a)] x) `shouldBe` a
    reduce (Let [("x", a)] $ Let [("y", x)] x) `shouldBe` a
  it "☯ environment propagation" $ do
    reduce (Let [("x", a)] $ Lam x y) `shouldBe` Lam x (Let [("x", a)] y)
    reduce (Let [("x", a)] $ Or x y) `shouldBe` Or (Let [("x", a)] x) (Let [("x", a)] y)
    reduce (Let [("x", a)] $ App x y) `shouldBe` App a (Let [("x", a)] y)
  it "☯ error propagation" $ do
    reduce (Let [("x", Err)] $ App x a) `shouldBe` Err
  it "☯ application" $ do
    reduce (Let [("x", Tup)] $ App x a) `shouldBe` App Tup (Let [("x", Tup)] a)
    reduce (Let [("x", Add)] $ App x i0) `shouldBe` App Add (Let [("x", Add)] i0)
    reduce (Let [("x", Sub)] $ App x i0) `shouldBe` App Sub (Let [("x", Sub)] i0)
    reduce (Let [("x", Mul)] $ App x i0) `shouldBe` App Mul (Let [("x", Mul)] i0)
    reduce (Let [("x", a)] $ App x b) `shouldBe` App a (Let [("x", a)] b)
    reduce (Let [("x", App a b)] $ App x c) `shouldBe` App (App a (Let [("x", App a b)] b)) (Let [("x", App a b)] c)
  it "☯ arithmetic reductions" $ do
    reduce (Let [("x", a)] $ add x x) `shouldBe` add a a
    reduce (Let [("x", i1)] $ add x x) `shouldBe` i2
    reduce (Let [("x", i1)] $ sub x x) `shouldBe` i0
    reduce (Let [("x", i1)] $ mul x x) `shouldBe` i1
  it "☯ error recovery" $ do
    reduce (Let [("x", Err)] $ App (Lam Err a) x) `shouldBe` a
  it "☯ constant matching" $ do
    reduce (Let [("x", a)] $ App (Lam a b) x) `shouldBe` b
    reduce (Let [("x", b)] $ App (Lam a b) x) `shouldBe` Err
  it "☯ variable binding" $ do
    reduce (Let [("x", a)] $ App (Lam y y) x) `shouldBe` a
  it "☯ pattern evaluation" $ do
    reduce (Let [("x", a)] $ App (Lam (Let [("y", a)] y) b) x) `shouldBe` b
  -- TODO: pattern Lam
  it "☯ application matching" $ do
    reduce (Let [("x", App a b)] $ App (Lam (App x y) x) x) `shouldBe` a
    reduce (Let [("x", App a b)] $ App (Lam (App x y) y) x) `shouldBe` b
    reduce (Let [("x", a)] $ App (Lam (App x y) y) x) `shouldBe` Err
  it "☯ argument reduction" $ do
    reduce (Let [("x", add i0 i1)] $ App (Lam i1 a) x) `shouldBe` a
    reduce (Let [("x", add i0 i1)] $ App (Lam i2 a) x) `shouldBe` Err
  it "☯ case alternation" $ do
    reduce (Let [("x", a)] $ App (Or (Lam a b) (Lam b c)) x) `shouldBe` b
    reduce (Let [("x", b)] $ App (Or (Lam a b) (Lam b c)) x) `shouldBe` c
    reduce (Let [("x", c)] $ App (Or (Lam a b) (Lam b c)) x) `shouldBe` Err
  it "☯ pattern alternation" $ do
    reduce (Let [("x", a)] $ App (Lam (Or a b) c) x) `shouldBe` c
    reduce (Let [("x", b)] $ App (Lam (Or a b) c) x) `shouldBe` c
    reduce (Let [("x", c)] $ App (Lam (Or a b) c) x) `shouldBe` Err

  it "☯ lam" $ do
    lam [] Tup `shouldBe` Tup
    lam [x] Tup `shouldBe` Lam x Tup
    lam [x, y, z] Tup `shouldBe` Lam x (Lam y (Lam z Tup))
  it "☯ app" $ do
    app Tup [] `shouldBe` Tup
    app Tup [x] `shouldBe` App Tup x
    app Tup [x, y, z] `shouldBe` App (App (App Tup x) y) z
  it "☯ add" $ do
    add x y `shouldBe` App (App Add x) y
  it "☯ sub" $ do
    sub x y `shouldBe` App (App Sub x) y
  it "☯ mul" $ do
    mul x y `shouldBe` App (App Mul x) y

  it "☯ factorial (simple recursion)" $ do
    -- factorial 0 = 1
    -- factorial n = n * factorial (n - 1)
    let f = Var "f"
    let n = Var "n"
    let case1 = Lam i0 i1
    let case2 = Lam n (mul n $ App f $ sub n i1)
    let env = [("f", Or case1 case2)]
    reduce (Let env (app f [Int 0])) `shouldBe` Int 1
    reduce (Let env (app f [Int 1])) `shouldBe` Int 1
  -- reduce (Let env (app f [Int 2])) `shouldBe` Int 2

  it "☯ ackermann (complex recursion)" $ do
    -- a 0 n = n + 1
    -- a m 0 = a (m - 1) 1
    -- a m n = a (m - 1) (a m (n - 1))
    -- let a = Var "a"
    -- let m = Var "m"
    -- let n = Var "n"
    -- let case1 = Lam i0 (For "n" $ Lam n $ add n i1)
    -- let case2 = For "m" (Lam m $ Lam i0 $ app a [sub m i1, i1])
    -- let case3 = For "m" (Lam m $ For "n" $ Lam n $ app a [sub m i1, app a [m, sub n i1]])
    -- let env = [("a", Or case1 $ Or case2 case3)]
    -- reduce (app a [i0]) env `shouldBe` Right (Lam n (add n i1), ("n", n) : env)
    -- reduce (app a [i1]) env `shouldBe` Right (Lam i0 $ app a [sub m i1, i1], ("m", Let (("m", m) : env) i1) : env)
    -- reduce (app a [i2]) env `shouldBe` Right (Lam i0 $ app a [sub m i1, i1], ("m", Let (("m", m) : env) i2) : env)
    -- reduce (app a [i0, i0]) env `shouldBe` Right (Int 1, ("n", Int 0) : env)
    -- reduce (app a [i0, i1]) env `shouldBe` Right (Int 2, ("n", Int 1) : env)
    -- reduce (app a [i0, i2]) env `shouldBe` Right (Int 3, ("n", Int 2) : env)
    -- reduce (app a [i1, i0]) env `shouldBe` Right (Int 2, ("n", Int 1) : ("m", Let (("m", m) : env) i1) : env)
    -- -- reduce (app a [i1, i1]) env `shouldBe` Right (Int 2, ("n", Int 1) : ("m", Let (("m", m) : env) i1) : env)
    True `shouldBe` True

-- TODO: map (parallelization)
-- TODO: foldr (lazy evaluation)
-- TODO: foldl (tail call optimization)
