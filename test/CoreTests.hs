module CoreTests where

import Core
import Test.Hspec

coreTests :: SpecWith ()
coreTests = describe "--== Core ==--" $ do
  let (i0, i1, i2) = (Int 0, Int 1, Int 2)
  let (x, y, z) = (Var "x", Var "y", Var "z")
  let (a, b, c) = (Ctr "A", Ctr "B", Ctr "C")

  it "☯ occurs" $ do
    occurs "x" (Err (Var "x")) `shouldBe` False
    occurs "x" (Var "x") `shouldBe` True
    occurs "x" (Var "y") `shouldBe` False
    occurs "x" (Ctr "x") `shouldBe` False
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

  it "☯ constants" $ do
    reduce (Err x) [] `shouldBe` (Err x, [])
    reduce i0 [] `shouldBe` (i0, [])
    reduce a [] `shouldBe` (a, [])
  it "☯ variables" $ do
    reduce x [("x", a)] `shouldBe` (a, [("x", a)])
    reduce y [("x", a)] `shouldBe` (Err y, [("x", a)])
  it "☯ closures" $ do
    reduce (Let [("x", a)] x) [("y", b)] `shouldBe` (a, [("y", b)])
    reduce (Let [("x", a)] y) [("y", b)] `shouldBe` (Err y, [("y", b)])
  it "☯ environment propagation" $ do
    reduce (Lam x y) [("x", a)] `shouldBe` (Lam x y, [("x", a)])
  -- reduce (Or x y) [("x", a)] `shouldBe` (Or x y, [("x", a)])
  it "☯ error propagation" $ do
    reduce (App x b) [("x", Err a)] `shouldBe` (Err (App a b), [("x", Err a)])
  it "☯ application" $ do
    reduce (App x a) [("x", Tup)] `shouldBe` (App Tup a, [("x", Tup)])
    reduce (App x i0) [("x", Add)] `shouldBe` (App Add i0, [("x", Add)])
    reduce (App x b) [("x", a)] `shouldBe` (App a b, [("x", a)])
    reduce (App x c) [("x", App a b)] `shouldBe` (App (App a b) c, [("x", App a b)])
  it "☯ arithmetic reductions" $ do
    reduce (add x x) [("x", a)] `shouldBe` (add a a, [("x", a)])
    reduce (add x x) [("x", i1)] `shouldBe` (i2, [("x", i1)])
    reduce (sub x x) [("x", i1)] `shouldBe` (i0, [("x", i1)])
    reduce (mul x x) [("x", i1)] `shouldBe` (i1, [("x", i1)])
  it "☯ error recovery" $ do
    reduce (App (Lam (Err a) b) x) [("x", Err c)] `shouldBe` (b, [("x", Err c)])
  it "☯ constant matching" $ do
    reduce (App (Lam a b) x) [("x", a)] `shouldBe` (b, [("x", a)])
    reduce (App (Lam a b) x) [("x", b)] `shouldBe` (Err b, [("x", b)])
  it "☯ variable binding" $ do
    reduce (App (Lam y y) x) [("x", a)] `shouldBe` (a, [("x", a)])
  it "☯ pattern evaluation" $ do
    reduce (App (Lam (Let [("y", a)] y) b) x) [("x", a)] `shouldBe` (b, [("x", a)])
  -- TODO: pattern Lam
  it "☯ application matching" $ do
    reduce (App (Lam (App x y) x) x) [("x", App a b)] `shouldBe` (a, [("x", App a b)])
    reduce (App (Lam (App x y) y) x) [("x", App a b)] `shouldBe` (b, [("x", App a b)])
    reduce (App (Lam (App x y) y) x) [("x", a)] `shouldBe` (Err a, [("x", a)])
  it "☯ argument reduction" $ do
    reduce (App (Lam i1 a) x) [("x", add i0 i1)] `shouldBe` (a, [("x", add i0 i1)])
    reduce (App (Lam i2 a) x) [("x", add i0 i1)] `shouldBe` (Err i1, [("x", add i0 i1)])
  it "☯ case alternation" $ do
    reduce (App (Or (Lam a b) (Lam b c)) x) [("x", a)] `shouldBe` (b, [("x", a)])
    reduce (App (Or (Lam a b) (Lam b c)) x) [("x", b)] `shouldBe` (c, [("x", b)])
    reduce (App (Or (Lam a b) (Lam b c)) x) [("x", c)] `shouldBe` (Err c, [("x", c)])
  it "☯ pattern alternation" $ do
    reduce (App (Lam (Or a b) c) x) [("x", a)] `shouldBe` (c, [("x", a)])
    reduce (App (Lam (Or a b) c) x) [("x", b)] `shouldBe` (c, [("x", b)])
    reduce (App (Lam (Or a b) c) x) [("x", c)] `shouldBe` (Err c, [("x", c)])

  it "☯ syntax sugar" $ do
    lam [] Tup `shouldBe` Tup
    lam [x] Tup `shouldBe` Lam x Tup
    lam [x, y, z] Tup `shouldBe` Lam x (Lam y (Lam z Tup))
    app Tup [] `shouldBe` Tup
    app Tup [x] `shouldBe` App Tup x
    app Tup [x, y, z] `shouldBe` App (App (App Tup x) y) z
    add x y `shouldBe` App (App Add x) y
    sub x y `shouldBe` App (App Sub x) y
    mul x y `shouldBe` App (App Mul x) y

  it "☯ factorial (simple recursion)" $ do
    -- f 0 = 1
    -- f n = n * f (n - 1)
    let f = Var "f"
    let n = Var "n"
    let case1 = Lam i0 i1
    let case2 = Lam n (mul n $ App f $ sub n i1)
    let env = [("f", Or case1 case2)]
    reduce (app f [Int 0]) env `shouldBe` (Int 1, env)
    reduce (app f [Int 1]) env `shouldBe` (Int 1, env)
    reduce (app f [Int 2]) env `shouldBe` (Int 2, env)
    reduce (app f [Int 5]) env `shouldBe` (Int 120, env)
    True `shouldBe` True

  -- env = {n = 2, f = 0 -> 1 | n -> n * f (n-1)}
  -- (n -> n * f (n-1)) (n-1)
  -- n = (n=2; f=..; n-1); n * f (n-1)

  it "☯ ackermann (complex recursion)" $ do
    -- a 0 n = n + 1
    -- a m 0 = a (m - 1) 1
    -- a m n = a (m - 1) (a m (n - 1))
    let a = Var "a"
    let m = Var "m"
    let n = Var "n"
    let case1 = Lam i0 (Lam n $ add n i1)
    let case2 = Lam m (Lam i0 $ app a [sub m i1, i1])
    let case3 = Lam m (Lam n $ app a [sub m i1, app a [m, sub n i1]])
    let env = [("a", Or case1 $ Or case2 case3)]
    reduce (app a [i0]) env `shouldBe` (Or (Lam n $ add n i1) (App (Or case2 case3) i0), env)
    reduce (app a [i1]) env `shouldBe` (Or (Lam i0 $ app a [sub m i1, i1]) (App case3 i1), env)
    reduce (app a [i2]) env `shouldBe` (Or (Lam i0 $ app a [sub m i1, i1]) (App case3 i2), env)
    reduce (app a [i0, i0]) env `shouldBe` (Int 1, env)
    reduce (app a [i0, i1]) env `shouldBe` (Int 2, env)
    reduce (app a [i0, i2]) env `shouldBe` (Int 3, env)
    -- reduce (app a [i1, i0]) env `shouldBe` (Int 2, env)
    -- reduce (app a [i1, i1]) env `shouldBe` (Int 2, env)
    True `shouldBe` True

-- TODO: map (parallelization)
-- TODO: foldr (lazy evaluation)
-- TODO: foldl (tail call optimization)
