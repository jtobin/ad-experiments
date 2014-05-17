-- Forward-mode AD in a very simple symbolic language.  Here using GADTs.
--
-- Can capture the body of a function via
--
-- >> example (Var "x")
-- Add (Add (Mul (Var "x") (Var "x")) (Mul (Lit 2.0) (Var "x"))) (Lit 1.0)
--
-- Can't observe sharing in loops:
--
-- >> tree (Var "x")
-- (infinite loop)
--
-- Can capture the body of a non-looping AD'd structure:
--
-- >> embed square (Var "x")
-- Dual {
--     primal = Mul (Var "x") (Var "x")
--   , tangent = Add (Mul (Var "x") (Lit 1.0)) (Mul (Var "x") (Lit 1.0))
--   }
--
-- Can't observe sharing in AD:
--
-- >> embed tree 1
-- (infinite loop)

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Syntax1 where

import Control.Applicative
import Data.Reify

data Expr where
  Lit :: Double -> Expr
  Add :: Expr -> Expr -> Expr
  Sub :: Expr -> Expr -> Expr
  Mul :: Expr -> Expr -> Expr
  Var :: String -> Expr
  deriving (Eq, Show)

data ExprF e =
    LitF Double
  | AddF e e
  | SubF e e
  | MulF e e
  | VarF String
  deriving (Eq, Show)

instance MuRef Expr where
  type DeRef Expr        = ExprF
  mapDeRef f (Add e0 e1) = AddF <$> f e0 <*> f e1
  mapDeRef f (Sub e0 e1) = SubF <$> f e0 <*> f e1
  mapDeRef f (Mul e0 e1) = MulF <$> f e0 <*> f e1
  mapDeRef _ (Lit v)     = pure (LitF v)
  mapDeRef _ (Var s)     = pure (VarF s)

instance Num Expr where
  fromInteger = Lit . fromInteger
  e0 + e1     = Add e0 e1
  e0 - e1     = Sub e0 e1
  e0 * e1     = Mul e0 e1
  signum      = undefined
  abs         = undefined

data Dual = Dual {
    primal  :: Expr
  , tangent :: Expr
  } deriving (Eq, Show)

instance Num Dual where
  fromInteger                 = constD . fromInteger
  (Dual e0 e1) + (Dual y0 y1) = Dual (e0 + y0) (e1 + y1)
  (Dual e0 e1) * (Dual y0 y1) = Dual (e0 * y0) (e0 * y1 + y0 * e1)
  negate (Dual e0 e1)         = Dual (negate e0) (negate e1)
  signum _                    = undefined
  abs _                       = undefined

eval :: Expr -> Double
eval (Lit j)     = j
eval (Add e0 e1) = eval e0 + eval e1
eval (Sub e0 e1) = eval e0 - eval e1
eval (Mul e0 e1) = eval e0 * eval e1
eval (Var _)     = error "evaluated unbound Var"

text :: Expr -> String
text (Lit j)     = show j
text (Add e0 e1) = "(" ++ text e0 ++ " + " ++ text e1 ++ ")"
text (Sub e0 e1) = "(" ++ text e0 ++ " - " ++ text e1 ++ ")"
text (Mul e0 e1) = "(" ++ text e0 ++ " * " ++ text e1 ++ ")"
text (Var x)     = x

constD :: Expr -> Dual
constD x = Dual x (Lit 0)

idD :: Expr -> Dual
idD x = Dual x (Lit 1.0)

evalDual :: Dual -> (Double, Double)
evalDual (Dual e0 e1) = (eval e0, eval e1)

embed :: (Dual -> c) -> Expr -> c
embed f = f . idD

simpleInc :: Num a => a -> a
simpleInc x = x + 1

example :: Num a => a -> a
example x = x ^ 2 + 2 * x + 1

square :: Num a => a -> a
square x = x * x

tree :: (Num a, Eq a) => a -> Expr
tree 0 = Lit 1
tree n =
  let shared = tree (n - 1)
  in  shared + shared

main :: IO ()
main = do
  print $ simpleInc (Var "x")     -- observes body of non-looping function
  print $ embed example (Var "x") -- observes body of non-looping AD'd function
  -- reifyGraph (tree (Var "x"))  -- can't observe loop structure
  -- print $ embed tree (Var "x") -- can't observe loop structure in AD'd f

