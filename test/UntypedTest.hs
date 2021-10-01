import Test.Hspec
import Untyped

main :: IO ()
main = hspec $ do
  describe "☯︎ Merge" $ do
    it "{x: 1} {x: 1}  ∴  {x: 1}" $ do
      merge [("x", Num 1)] [("x", Num 1)] `shouldBe` [("x", Num 1)]
    it "{x: 1} {x: 2}  ∴  {x: 1}" $ do
      merge [("x", Num 1)] [("x", Num 2)] `shouldBe` [("x", Num 2)]
    it "{x: 1} {y: 2}  ∴  {x: 1, y: 2}" $ do
      merge [("x", Num 1)] [("y", Num 2)] `shouldBe` [("x", Num 1), ("y", Num 2)]
    it "{x: 1, y: 2} {z: 3, w: 4}  ∴  {x: 1, y: 2}" $ do
      merge [("x", Num 1), ("y", Num 2)] [("z", Num 3), ("w", Num 4)] `shouldBe` [("x", Num 1), ("y", Num 2), ("z", Num 3), ("w", Num 4)]

  describe "☯︎ Number" $ do
    it "1  ∴  1" $ do
      eval (Num 1) [] `shouldBe` Ok (Num 1)

  describe "☯︎ Variable" $ do
    it "x  ∴  Undefined variable: x" $ do
      eval (Var "x") [] `shouldBe` Err (UndefinedVar "x")
    it "x  Γ{x: 1}  ∴  1" $ do
      eval (Var "x") [("x", Num 1)] `shouldBe` Ok (Num 1)
    it "x  Γ{x: x}  ∴  x" $ do
      eval (Var "x") [("x", Var "x")] `shouldBe` Ok (Var "x")
    it "x  Γ{y: 1, x: y}  ∴  1" $ do
      eval (Var "x") [("y", Num 1), ("x", Var "y")] `shouldBe` Ok (Num 1)
  -- it "x  Γ{x: λy. y}  ∴  λy. y" $ do
  --   eval (Var "x") [("x", Lam "y" (Var "y"))] `shouldBe` Ok (Lam "y" (Var "y"))
  -- it "x  Γ{x: f = f}  ∴  f = f" $ do
  --   eval (Var "x") [("x", Rec "f" (Var "f"))] `shouldBe` Ok (Rec "f" (Var "f"))

  -- describe "☯︎ Rec bindings" $ do
  --   it "{} 1  ∴  1" $ do
  --     eval (Rec [] (Num 1)) [] `shouldBe` Ok (Num 1) []
  --   it "{} x  ∴  Undefined variable: x" $ do
  --     eval (Rec [] (Var "x")) [] `shouldBe` Err (UndefinedVar "x")
  --   it "{} ({} 1)  ∴  1" $ do
  --     eval (Rec [] (Rec [] (Num 1))) [] `shouldBe` Ok (Num 1) []
  --   it "{} (λx. 1)  ∴  λx. 1" $ do
  --     eval (Rec [] (Lam "x" (Num 1))) [] `shouldBe` Ok (Lam "x" (Num 1)) []

  describe "☯︎ Recursive definition" $ do
    it "x = x  ∴  x = x" $ do
      eval (Rec "x" (Var "x")) [] `shouldBe` Ok (Rec "x" (Var "x"))
    it "x = 1  ∴  1" $ do
      eval (Rec "f" (Num 1)) [] `shouldBe` Ok (Num 1)

  describe "☯︎ λ Abstraction" $ do
    it "λx. y  ∴  Undefined variable: y" $ do
      eval (Lam "x" (Var "y")) [] `shouldBe` Err (UndefinedVar "y")
    it "λx. 1  ∴  λx. 1" $ do
      eval (Lam "x" (Num 1)) [] `shouldBe` Ok (Lam "x" (Num 1))
    it "λx. x  ∴  λx. x" $ do
      eval (Lam "x" (Var "x")) [] `shouldBe` Ok (Lam "x" (Var "x"))
    it "λx. y  Γ{y: y}  ∴  λx. y" $ do
      eval (Lam "x" (Var "y")) [("y", Var "y")] `shouldBe` Ok (Lam "x" (Var "y"))

  describe "☯︎ Application" $ do
    it "1 2  ∴  Not a function" $ do
      eval (App (Num 1) (Num 2)) [] `shouldBe` Err (NotAFunction (Num 1))
    it "f x  ∴  Undefined variable: f" $ do
      eval (App (Var "f") (Var "x")) [] `shouldBe` Err (UndefinedVar "f")
    it "f x  Γ{f: f}  ∴  Undefined variable: x" $ do
      eval (App (Var "f") (Var "x")) [("f", Var "f")] `shouldBe` Err (UndefinedVar "x")
    it "f 1  Γ{f: f}  ∴  f 1" $ do
      eval (App (Var "f") (Num 1)) [("f", Var "f")] `shouldBe` Ok (App (Var "f") (Num 1))
    it "f x  Γ{f: f, x: x}  ∴  f x" $ do
      eval (App (Var "f") (Var "x")) [("f", Var "f"), ("x", Var "x")] `shouldBe` Ok (App (Var "f") (Var "x"))
    it "(f = f) 1  ∴  f = f 1" $ do
      eval (App (Rec "f" (Var "f")) (Num 1)) [] `shouldBe` Ok (Rec "f" (App (Var "f") (Num 1)))
    it "(λx. 1) 2  ∴  1" $ do
      eval (App (Lam "x" (Num 1)) (Num 2)) [] `shouldBe` Ok (Num 1)
    it "(λx. x) 1  ∴  1" $ do
      eval (App (Lam "x" (Var "x")) (Num 1)) [] `shouldBe` Ok (Num 1)
    it "f 2  Γ{f: λx. 1}  ∴  1" $ do
      eval (App (Var "f") (Num 2)) [("f", Lam "x" (Num 1))] `shouldBe` Ok (Num 1)
    it "((λx. x) (λy. y)) 1  ∴  1" $ do
      eval (App (App (Lam "x" (Var "x")) (Lam "y" (Var "y"))) (Num 1)) [] `shouldBe` Ok (Num 1)
  --   it "((λx. x) (λy. x)) 1  ∴  1" $ do
  --     eval (App (App (Lam "x" (Var "x")) (Lam "y" (Var "x"))) (Num 1)) [] `shouldBe` Ok (Num 1)

  describe "☯︎ Addition" $ do
    it "(+)  ∴  (+)" $ do
      eval Add [] `shouldBe` Ok Add
    it "(+) x  Γ{x: x}  ∴  (+) x" $ do
      eval (App Add (Var "x")) [("x", Var "x")] `shouldBe` Ok (App Add (Var "x"))
    it "(+) x y  Γ{x: x, y: y}  ∴  x + y" $ do
      eval (add (Var "x") (Var "y")) [("x", Var "x"), ("y", Var "y")] `shouldBe` Ok (add (Var "x") (Var "y"))
    it "(+) x y  Γ{x: 1, y: 2}  ∴  3" $ do
      eval (add (Var "x") (Var "y")) [("x", Num 1), ("y", Num 2)] `shouldBe` Ok (Num 3)

  describe "☯︎ Subtraction" $ do
    it "(-)  ∴  (-)" $ do
      eval Sub [] `shouldBe` Ok Sub
    it "(-) x  Γ{x: x}  ∴  (-) x" $ do
      eval (App Sub (Var "x")) [("x", Var "x")] `shouldBe` Ok (App Sub (Var "x"))
    it "(-) x y  Γ{x: x, y: y}  ∴  x - y  Γ{x: x, y: y}" $ do
      eval (sub (Var "x") (Var "y")) [("x", Var "x"), ("y", Var "y")] `shouldBe` Ok (sub (Var "x") (Var "y"))
    it "(-) x y  Γ{x: 1, y: 2}  ∴  -1" $ do
      eval (sub (Var "x") (Var "y")) [("x", Num 1), ("y", Num 2)] `shouldBe` Ok (Num (-1))

  describe "☯︎ Multiplication" $ do
    it "(*)  ∴  (*)" $ do
      eval Mul [] `shouldBe` Ok Mul
    it "(*) x  Γ{x: x}  ∴  (*) x" $ do
      eval (App Mul (Var "x")) [("x", Var "x")] `shouldBe` Ok (App Mul (Var "x"))
    it "(*) x y  Γ{x: x, y: y}  ∴  x * y  Γ{x: x, y: y}" $ do
      eval (mul (Var "x") (Var "y")) [("x", Var "x"), ("y", Var "y")] `shouldBe` Ok (mul (Var "x") (Var "y"))
    it "(*) x y  Γ{x: 1, y: 2}  ∴  2" $ do
      eval (mul (Var "x") (Var "y")) [("x", Num 1), ("y", Num 2)] `shouldBe` Ok (Num 2)

  describe "☯︎ Equals" $ do
    it "(==)  ∴  (==)" $ do
      eval Eq [] `shouldBe` Ok Eq
    it "(==) x  Γ{x: x}  ∴  (==) x" $ do
      eval (App Eq (Var "x")) [("x", Var "x")] `shouldBe` Ok (App Eq (Var "x"))
    it "(==) x y  Γ{x: x, y: y}  ∴  x == y  Γ{x: x, y: y}" $ do
      eval (eq (Var "x") (Var "y")) [("x", Var "x"), ("y", Var "y")] `shouldBe` Ok (eq (Var "x") (Var "y"))
    it "(==) x y  Γ{x: 1, y: 2}  ∴  λTrue False. False" $ do
      eval (eq (Var "x") (Var "y")) [("x", Num 1), ("y", Num 2)] `shouldBe` Ok (Lam "True" (Lam "False" (Var "False")))
    it "(==) x y  Γ{x: 2, y: 2}  ∴  λTrue False. False" $ do
      eval (eq (Var "x") (Var "y")) [("x", Num 2), ("y", Num 2)] `shouldBe` Ok (Lam "True" (Lam "False" (Var "True")))
    it "(==) x y  Γ{x: 3, y: 2}  ∴  λTrue False. False" $ do
      eval (eq (Var "x") (Var "y")) [("x", Num 3), ("y", Num 2)] `shouldBe` Ok (Lam "True" (Lam "False" (Var "False")))

  describe "☯︎ Simple recursion" $ do
    it "f  Γ{f: λx. f x}  ∴  f  f = λx. f x" $ do
      eval (Var "f") [("f", Lam "x" (App (Var "f") (Var "x")))] `shouldBe` Ok (Rec "f" (Lam "x" (App (Var "f") (Var "x"))))
    it "f x  Γ{f: λx. f x, x: x}  ∴  f = f x" $ do
      eval (App (Var "f") (Var "x")) [("f", Lam "x" (App (Var "f") (Var "x"))), ("x", Var "x")] `shouldBe` Ok (Rec "f" (App (Var "f") (Var "x")))

  describe "☯︎ Factorial" $ do
    it "f  Γ{f: factorial}  ∴  recursive definition" $ do
      eval (Var "f") [("f", factorial)] `shouldBe` Ok (Lam "n" (app (eq n k0) [k1, mul n (Rec "f" (App f (sub n k1)))]))
    it "f n  Γ{f: factorial, n: n}  ∴  recursive application" $ do
      eval (App (Var "f") (Var "n")) [("f", factorial), ("n", Var "n")] `shouldBe` Ok (app (eq n k0) [k1, mul n (Rec "f" (App f (sub n k1)))])
    it "f 0  Γ{f: factorial}  ∴  1" $ do
      eval (App (Var "f") (Num 0)) [("f", factorial)] `shouldBe` Ok (Num 1)
  --   it "f 1  Γ{f: factorial}  ∴  1" $ do
  --     eval (App (Var "f") (Num 1)) [("f", factorial)] `shouldBe` Ok (Num 1)
  --   it "f 2  Γ{f: factorial}  ∴  2" $ do
  --     eval (App (Var "f") (Num 2)) [("f", factorial)] `shouldBe` Ok (Num 2)
  --   it "f 5  Γ{f: factorial}  ∴  120" $ do
  --     eval (App (Var "f") (Num 5)) [("f", factorial)] `shouldBe` Ok (Num 120)

  -- describe "☯︎ Ackermann" $ do
  --   it "a  Γ{a: ackermann}  ∴  {a: ackermann} a" $ do
  --     eval (Var "a") [("a", ackermann)] `shouldBe` Ok (Rec [("a", ackermann)] (Var "a"))
  --   it "a m  Γ{a: ackermann, m: m}  ∴  {a: ackermann, m: m} a m" $ do
  --     eval (App (Var "a") (Var "m")) [("a", ackermann), ("m", Var "m")] `shouldBe` Ok (Rec [("a", ackermann), ("m", Var "m")] (App (Var "a") (Var "m")))
  --   it "a 0  Γ{a: ackermann}  ∴  {a: ackermann} a 0" $ do
  --     eval (App (Var "a") (Num 0)) [("a", ackermann)] `shouldBe` Ok (Rec [("a", ackermann)] (App (Var "a") (Num 0)))
  --   it "a 0 0  Γ{a: ackermann}  ∴  1" $ do
  --     eval (app (Var "a") [Num 0, Num 0]) [("a", ackermann)] `shouldBe` Ok (Num 1)

  it "asdf" $ do 1 `shouldBe` 1
  where
    a = Var "a"
    f = Var "f"
    m = Var "m"
    n = Var "n"
    k0 = Num 0
    k1 = Num 1

    -- f 0 = 1
    -- f n = n * f (n - 1)
    factorial =
      Lam "n" (app (eq n k0) [k1, mul n (App f (sub n k1))])

    -- a 0 n = n + 1
    -- a m 0 = a (m-1) 1
    -- a m n = a (m-1) (a m (n-1))
    ackermann =
      Lam "m" (Lam "n" (app (eq m k0) [add n k1, app (eq n k0) [app a [sub m k1, k1], app a [sub m k1, app a [m, sub n k1]]]]))
