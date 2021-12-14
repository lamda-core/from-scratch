module Typed where

import Prelude

import Data.Array ((!!))
import Data.Generic.Rep (class Generic)
import Data.List (List(..), elem, foldl, (:))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Dict (Dict, KV(..), get, getOr, set)
import Dict as Dict
import Result (Result(..))
import Tuples (T2(..), T3(..), first, second)

-- https://github.com/kritzcreek/fby19
-- https://github.com/mroman42/mikrokosmos/blob/master/source/Stlc/Types.hs

data Expr
  = Any               -- _
  | TypT              -- Type
  | IntT              -- Int
  | Int Int           -- 42
  | Var String        -- x
  | Ctr String        -- A
  -- | Fix String Expr -- @x. a
  -- | For String Expr   -- ∀x. a
  | Ann Expr Typ      -- a : t
  | To  Expr Expr     -- a -> b
  | Lam Expr Expr     -- λp -> a
  | Or  Expr Expr     -- a | b
  | And Expr Expr     -- (a, b)
  | App Expr Expr     -- a b
  | Case Matcher Cases  -- $LR {A: 0, _: 1} [x=$LR; a, y=$; b]
  -- | Add               -- (+)
  -- | Sub               -- (-)
  -- | Mul               -- (*)

type Cases = Array (T2 (Dict String (List Decons)) Expr)

data Matcher
  = IntSwitch (List Decons) (List (T2 Int Matcher)) Matcher
  | CtrSwitch (List Decons) (List (T2 String Matcher)) (Maybe Matcher)
  | Then Int

data Pattern
  = PAny
  | PInt Int
  | PCtr String

data Decons
  = AnnL
  | AnnR
  | ToL
  | ToR
  | OrL
  | OrR
  | AndL
  | AndR
  | AppL
  | AppR

type Typ = Expr
type Env =
  { vars :: Dict String Expr
  , alts :: Dict String (List String)
  }
type Substitution = Expr -> Expr

data Error
  = UndefinedName String
  | NotAFunction Expr Typ
  | NotATaggedUnion Typ
  | TypeMismatch Typ Typ
  | UndefinedCase Int
  | MissingCases (List Pattern)
  | RedundantIntCases (List Int)

derive instance Eq Expr
derive instance Generic Expr _
instance Show Expr where
  show Any = "_"
  show TypT = "Type"
  show IntT = "Int"
  show (Int k) = show k
  show (Var x) = x
  show (Ctr x) = x
  -- show (For x e) = "∀" <> x <> ". " <> show e
  show (Ann e t) = "(" <> show e <> " : " <> show t <> ")"
  show (To p e) = "(" <> show p <> " -> " <> show e <> ")"
  show (Lam p a) = "(λ" <> show p <> " -> " <> show a <> ")"
  show (Or a b) = "(" <> show a <> " | " <> show b <> ")"
  show (And e1 e2) = "(" <> show e1 <> ", " <> show e2 <> ")"
  -- show (App (App Add e1) e2) = "(" <> show e1 <> " + " <> show e2 <> ")"
  -- show (App (App Sub e1) e2) = "(" <> show e1 <> " - " <> show e2 <> ")"
  -- show (App (App Mul e1) e2) = "(" <> show e1 <> " * " <> show e2 <> ")"
  show (App e1 e2) = "(" <> show e1 <> " " <> show e2 <> ")"
  show (Case matcher cases) = "TODO: Case"
  -- show Add = "(+)"
  -- show Sub = "(-)"
  -- show Mul = "(*)"
  -- show x = genericShow x

derive instance Eq Pattern
derive instance Generic Pattern _
instance Show Pattern where
  show PAny = "_"
  show (PInt k) = show k
  show (PCtr x) = x

derive instance Eq Decons
derive instance Generic Decons _
instance Show Decons where
  show AnnL = ":L"
  show AnnR = ":R"
  show ToL  = ">L"
  show ToR  = ">R"
  show OrL  = "|L"
  show OrR  = "|R"
  show AndL = ",L"
  show AndR = ",R"
  show AppL = " L"
  show AppR = " R"

derive instance Eq Matcher
derive instance Generic Matcher _
instance Show Matcher where
  show (IntSwitch decons mappings default) = "TODO: Matcher"
  show (CtrSwitch decons mappings maybeDefault) = "TODO: Matcher"
  show (Then i) = show i

derive instance Eq Error
derive instance Generic Error _
instance Show Error where
  show = genericShow

empty :: Env
empty = {vars : Dict.empty, alts : Dict.empty}

define :: String -> Expr -> Env -> Env
define name value {vars, alts} = {vars : set name value vars, alts}

defineCtr :: String -> String -> Typ -> Env -> Env
defineCtr typeName ctr typ {vars, alts} = do
  let alts' = set typeName (ctr : getOr typeName alts Nil) alts
  define ctr (Ann (Var ctr) typ) {vars, alts : alts'}

defineType :: String -> Typ -> Dict String Typ -> Env -> Env
defineType typeName kind (KV ctr typ : ctrs) env =
  defineType typeName kind ctrs (defineCtr typeName ctr typ env)
defineType typeName kind Nil env = define typeName kind env

app2 :: Expr -> Expr -> Expr -> Expr
app2 f e1 e2 = App (App f e1) e2

-- add2 :: Expr -> Expr -> Expr
-- add2 = app2 Add

-- sub2 :: Expr -> Expr -> Expr
-- sub2 = app2 Sub

-- mul2 :: Expr -> Expr -> Expr
-- mul2 = app2 Mul

-- Check must:
--  * Missing cases: each Switch must check the type of Patterns and make sure that all the constructors are included
--  * Undefined case: case index is out of bounds

withBindings :: Dict String (List Decons) -> Env -> Env
-- TODO: make sure all new types are unique
withBindings (KV x _ : bindings) env = withBindings bindings (define x (Var x) env)
withBindings Nil env = env

typeAlts :: Typ -> Dict String (List String) -> Result Error (List String)
typeAlts (Var x) envAlts = case get x envAlts of
  Just alts -> Ok alts
  Nothing -> Err (NotATaggedUnion (Var x))
typeAlts (App a _) envAlts = typeAlts a envAlts
typeAlts t _ = Err (NotATaggedUnion t)

allCasesOf :: Typ -> Dict String (List String) -> List Pattern
allCasesOf (Var x) alternatives = case get x alternatives of
  Just alts -> map PCtr alts
  Nothing -> PAny : Nil
allCasesOf _ _ = PAny : Nil

patternType :: Pattern -> Env -> Result Error Typ
patternType PAny _ = Ok Any
patternType (PInt _) _ = Ok IntT
patternType (PCtr x) env = Err (UndefinedName $ "Not implemented! patternType Ctr")

deconsType :: List Decons -> Typ -> Typ
deconsType (AndL : decons) t = And (deconsType decons t) Any
deconsType (AndR : decons) t = And Any (deconsType decons t)
deconsType Nil t = t
deconsType decons _ = Var $ "Not implemented! deconsType " <> show decons

duplicates :: forall a. Eq a => List a -> List a
-- TODO (optimization): remove `x`` from `xs` if it wasn't found to not check it again in `duplicate xs`
duplicates (x : xs) | x `elem` xs = x : duplicates xs
duplicates (_ : xs) = duplicates xs
duplicates Nil = Nil

checkIntSwitch :: List (T2 Int Matcher) -> Matcher -> Cases -> Env -> Result Error (T2 Typ Substitution)
checkIntSwitch mappings default cases env =
  case duplicates (map first mappings) of
    Nil -> checkMatchers (default : map second mappings) cases env
    dups -> Err (RedundantIntCases dups)

checkMatchers :: List Matcher -> Cases -> Env -> Result Error (T2 Typ Substitution)
checkMatchers matchers cases env =
  let
    checkMatcher result matcher = do
      T2 ta sa <- result
      T2 tb sb <- checkCase matcher cases env
      T2 tc sc <- unify (sb ta) (sa tb)
      Ok (T2 tc (sc <<< sb <<< sa))
  in foldl checkMatcher (Ok (T2 Any identity)) matchers

checkCase :: Matcher -> Cases -> Env -> Result Error (T2 Typ Substitution)
-- checkCase (Switch decons mappings) cases env = do
--   T3 tp tb s <- checkSwitch mappings cases env
--   Ok (T2 (deconsType decons tp `To` tb) s)
checkCase (IntSwitch decons mappings default) cases env = do
  T2 t s <- checkIntSwitch mappings default cases env
  Ok (T2 (deconsType decons IntT `To` t) s)
checkCase (Then i) cases env = case cases !! i of
  Just (T2 bindings a) -> check a (withBindings bindings env)
  Nothing -> Err (UndefinedCase i)
checkCase _ _ _ = Err (UndefinedName "Not implemented: checkCase")

------

occurs :: String -> Expr -> Boolean
occurs x (Var y)     = x == y
-- TODO: occurs x (Fix x' e)
-- occurs x (For y e) | x /= y = x `occurs` e
occurs x (Ann e t) = x `occurs` e || x `occurs` t
occurs x (To  a b) = x `occurs` a || x `occurs` b
-- occurs x (Lam (T2 p a : cases)) = not (x `occursP` p) && x `occurs` a || x `occurs` Lam cases
occurs x (Or  a b) = x `occurs` a || x `occurs` b
-- occurs x (And a b) = x `occurs` a || x `occurs` b
occurs x (App a b) = x `occurs` a || x `occurs` b
occurs _ _ = false

bindVar :: String -> Typ -> Substitution
bindVar x (Var x') t | x == x' = t
bindVar x t (Var x') | x == x' = t
bindVar x t (Ann a b) = Ann (bindVar x t a) (bindVar x t b)
-- TODO: bindVar x t (Lam)
bindVar x t (To  a b) = To  (bindVar x t a) (bindVar x t b)
bindVar x t (Or  a b) = Or  (bindVar x t a) (bindVar x t b)
-- bindVar x t (And a b) = And (bindVar x t a) (bindVar x t b)
bindVar x t (App a b) = App (bindVar x t a) (bindVar x t b)
bindVar _ _ t = t

unify :: Expr -> Expr -> Result Error (T2 Expr Substitution)
unify (Ann a1 b1) (Ann a2 b2) = unifyPair Ann a1 b1 a2 b2
unify (To  a1 b1) (To  a2 b2) = unifyPair To  a1 b1 a2 b2
unify (Or  a1 b1) (Or  a2 b2) = unifyPair Or  a1 b1 a2 b2
-- unify (And a1 b1) (And a2 b2) = unifyPair And a1 b1 a2 b2
unify (App a1 b1) (App a2 b2) = unifyPair App a1 b1 a2 b2
unify a a' | a == a' = Ok (T2 a identity)
unify Any b = Ok (T2 b identity)
unify a Any = Ok (T2 a identity)
unify (Var x) b | x `occurs` b = Err (TypeMismatch (Var x) b)
unify (Var x) b                = Ok (T2 b (bindVar x b))
unify a (Var x) | x `occurs` a = Err (TypeMismatch a (Var x))
unify a (Var x)                = Ok (T2 a (bindVar x a))
unify a b = Err (TypeMismatch a b)

unifyPair :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr -> Expr -> Result Error (T2 Expr Substitution)
unifyPair f a1 b1 a2 b2 = do
  T2 ta sa <- unify a1 a2
  T2 tb sb <- unify (sa b1) (sa b2)
  Ok (T2 (f (sb ta) (sa tb)) (sb <<< sa))

check :: Expr -> Env -> Result Error (T2 Typ Substitution)
check Any _ = Ok (T2 Any identity)
check TypT _ = Ok (T2 TypT identity)
check IntT _ = Ok (T2 TypT identity)
check (Int _) _ = Ok (T2 IntT identity)
check (Var x) env = case get x env.vars of
  Just (Var x') | x == x' -> Ok (T2 (Var x) identity)
  Just (Ctr x') | x == x' -> Ok (T2 (Var x) identity)
  Just (Ann (Var x') t) | x == x' -> Ok (T2 t identity)
  Just (Ann (Ctr x') t) | x == x' -> Ok (T2 t identity)
  Just a -> check a env
  Nothing -> Err (UndefinedName x)
check (Ctr x) env = check (Var x) env
check (Ann a t) env = do
  T2 ta sa <- check a env
  T2 t' s <- unify ta (sa t)
  Ok (T2 t' (s <<< sa))
-- check (To a b) env = do
--   let env' = declare a env
--   T2 ta sa <- check a env'
--   T2 tb sb <- check b env'
--   Ok (T2 (sb ta `To` sa tb) (sb <<< sa))
-- check (Lam Nil) env = Err (UndefinedName ("Not implemented! Lam []"))
-- check (Lam (T2 p a : cases)) env = Err (UndefinedName ("Not implemented! Lam (:)"))
-- check (Or a b) env = do
--   T2 tb sb <- check b env
--   T2 ta sa <- check (Ann a tb) env
--   Ok (T2 ta (sa <<< sb))
-- check (And a b) env = do
--   T2 ta sa <- check a env
--   T2 tb sb <- check b env
--   Ok (T2 (sb ta `And` sa tb) (sb <<< sa))
-- check (App a b) env = do
--   T2 ta sa <- check a (declare a env)
--   case ta of
--     To t1 t2 -> do
--       T2 _ sb <- check (Ann b t1) env
--       Ok (T2 (sb t2) (sb <<< sa))
--     _ -> Err (NotAFunction a ta)
-- check Add _ = Ok (T2 (Var "a" `To` (Var "a" `To` Var "a")) identity)
-- check Sub _ = Ok (T2 (Var "a" `To` (Var "a" `To` Var "a")) identity)
-- check Mul _ = Ok (T2 (Var "a" `To` (Var "a" `To` Var "a")) identity)
check a env = Err (UndefinedName $ "Not implemented! check " <> show a)

-- -- TODO: make Env implicit (?)
-- eval :: Expr -> Env -> Result Error (T2 Expr Typ)
-- -- eval expression environment = do
-- --   T3 e t _ <- eval' expression environment
-- --   Ok (T2 e t)
-- eval (Var x) env = case get x env of
--   Just (Var x') | x == x' -> Ok (T2 (Var x) (Var x))
--   -- Just (Ann (Var x') t) | x == x' -> Ok (T2 (Var x) t)
--   Just a -> eval a env
--   Nothing -> Err (UndefinedName x)
-- -- eval (Ann a t) env = do
-- --   T2 a' _ <- eval a env
-- --   T2 t' _ <- check (Ann a' t) env
-- --   Ok (T2 a' t')
-- -- eval (To a b) env = do
-- --   T2 a' _ <- eval a env
-- --   T2 b' _ <- eval b env
-- --   T2 t  _ <- check (To a b) env
-- --   Ok (T2 (To a' b') t)
-- eval a env = do
--   T2 ta _ <- check a env
--   Ok (T2 a ta)

-- eval' :: Expr -> Env -> Result Error (T3 Expr Typ Substitution)
-- eval' TypT _ = Ok (T3 TypT TypT identity)
-- eval' IntT _ = Ok (T3 IntT TypT identity)
-- eval' (Int k) _ = Ok (T3 (Int k) IntT identity)
-- eval' (Var x) env = case get x env of
--   -- TODO: validate (?)
--   Just (T2 a t) -> Ok (T3 a t identity)
--   Nothing -> Err (UndefinedName x)
-- -- eval' (Ctr x) env = case get x env of
-- --   Just t -> do
-- --     T3 t' _ s <- eval' t env
-- --     Ok (T3 (Ctr x) t' s)
-- --   Nothing -> Err (UndefinedName x)
-- -- TODO: Fix x a
-- -- eval' (For x a) env = do
-- --   T3 a' t s <- eval' a (KV x (Var x) : env)
-- --   if x `occurs` a'
-- --     then Ok (T3 (For x a') (For x t) s)
-- --     else Ok (T3 a' t s)
-- eval' (Ann a t) env = do
--   T3 a' ta sa <- eval' a env
--   T3 t' _  _  <- eval' (sa t) env
--   case unify ta t' of
--     Just s -> Ok (T3 a' t' (s <<< sa))
--     Nothing -> Err (TypeMismatch ta (sa t'))
-- eval' (To p a) env = do
--   let env' = declare p env
--   T3 p' tp _ <- eval' p env'
--   T3 a' ta s <- eval' a env'
--   Ok (T3 (p' `To` a') (s tp `To` ta) s)
-- eval' (Or a b) env = do
--   T3 a' ta sa <- eval' a env
--   T3 b' tb sb <- eval' b env
--   case unify (sb ta) (sa tb) of
--     Just s -> Ok (T3 (a' `Or` b') (s ta) (s <<< sb <<< sa))
--     Nothing -> Err (TypeMismatch (sb ta) (sa tb))
-- eval' (And a b) env = do
--   T3 a' ta sa <- eval' a env
--   T3 b' tb sb <- eval' b env
--   Ok (T3 (a' `And` b') (sb ta `And` sa tb) (sb <<< sa))
-- -- eval' (App (For x a) b) env = eval' (App a b) (KV x (Var x) : env)
-- eval' (App (To p a) b) env = do
--   T3 p' tp _  <- eval' p (declare p env)
--   T3 b' tb sb <- eval' b env
--   -- case T2 (unify p' b') (unify tp tb) of
--   --   T2 (Just vars) (Just s) -> do
--   --     T3 a' ta sa <- eval' (vars a) env
--   --     Ok (T3 a' ta (sa <<< s <<< sb))
--   --   T2 _ Nothing -> Err (TypeMismatch tp tb)
--   --   T2 Nothing _ -> Err (PatternMismatch p b')
--   case unify tp tb of
--     Just s -> case match p' b' of
--       Just vars -> do
--         T3 a' ta sa <- eval' (vars a) env
--         Ok (T3 a' ta (sa <<< s <<< sb))
--       Nothing -> Err (PatternMismatch p b')
--     Nothing -> Err (TypeMismatch (sb tp) tb)
-- eval' (App (Or a b) c) env = do
--   -- TODO: validate Or and type-check
--   case eval' (App a c) env of
--     Err (PatternMismatch _ _) -> eval' (App b c) env
--     result -> result
-- eval' (App (And a b) c) env = do
--   T3 a' ta sa <- eval' (App a c) env
--   T3 b' tb sb <- eval' (App b c) env
--   Ok (T3 (a' `And` b') (sb ta `And` sa tb) (sb <<< sa))
-- eval' (App a b) env = do
--   T3 a' ta sa <- eval' a env
--   T3 b' tb sb <- eval' b env
--   case sb ta of
--     To t1 t2 -> case unify t1 tb of
--       Just s -> case T2 a' b' of
--         T2 (App Add (Int k1)) (Int k2) -> Ok (T3 (Int (k1 + k2)) IntT (s <<< sb <<< sa))
--         T2 (App Sub (Int k1)) (Int k2) -> Ok (T3 (Int (k1 - k2)) IntT (s <<< sb <<< sa))
--         T2 (App Mul (Int k1)) (Int k2) -> Ok (T3 (Int (k1 * k2)) IntT (s <<< sb <<< sa))
--         _ | a == a' -> Ok (T3 (App a' b') (s t2) (s <<< sb <<< sa))
--         _ -> eval' (App a' b') env
--       Nothing -> Err (TypeMismatch t1 (sa tb))
--     _ -> Err (NotAFunction a')
-- eval' Add _ = Ok (T3 Add (Var "a" `To` (Var "a" `To` Var "a")) identity)
-- eval' Sub _ = Ok (T3 Sub (Var "a" `To` (Var "a" `To` Var "a")) identity)
-- eval' Mul _ = Ok (T3 Mul (Var "a" `To` (Var "a" `To` Var "a")) identity)

----------------------------

-- substitute :: String -> Expr -> Expr -> Expr
-- substitute x ex (Var x') | x == x' = ex
-- substitute x ex (For y e) | x /= y = For y (substitute x ex e)
-- substitute x ex (Ann e t) = Ann (substitute x ex e) (substitute x ex t)
-- substitute x ex (To e1 e2) = To (substitute x ex e1) (substitute x ex e2)
-- substitute x ex (Or e1 e2) = Or (substitute x ex e1) (substitute x ex e2)
-- substitute x ex (And e1 e2) = And (substitute x ex e1) (substitute x ex e2)
-- substitute x ex (App e1 e2) = App (substitute x ex e1) (substitute x ex e2)
-- substitute _ _ e = e
