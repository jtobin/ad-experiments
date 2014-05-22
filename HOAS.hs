{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module HOAS where

import Utils

data Expr =
    Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Let Expr (Expr -> Expr)

let_ :: Expr -> (Expr -> Expr) -> Expr
let_ e0 e1 = Let e0 (\x -> e1 x)

instance Num Expr where
  fromInteger = Lit . fromInteger
  e0 + e1     = Add e0 e1
  e0 - e1     = Sub e0 e1
  e0 * e1     = Mul e0 e1
  signum      = undefined
  abs         = undefined

instance Eq Expr where
  e0 == e1 = eval e0 == eval e1

-- need a function to convert the shared expression from the host back into the
-- embedded language; here just Lit
--
-- so you can imagine doing case analysis on the shared expression:
--
--   let shared = case eval e0 of
--     something    -> Lit something
--     anotherThing -> Var somethingElse
eval :: Expr -> Int
eval (Lit d)     = d
eval (Add e0 e1) = eval e0 + eval e1
eval (Sub e0 e1) = eval e0 - eval e1
eval (Mul e0 e1) = eval e0 * eval e1
eval (Let e0 e1) = let shared = Lit (eval e0)
                   in  eval (e1 shared)

-- problem here in expressions with let statements; e1 :: Expr -> Expr but
-- v is String.  need an interpreter to quote that back into the language.
-- PHOAS takes care of this automatically.
-- text :: Expr -> String
-- text e = go e 0 where
--   go (Lit j)     _ = show j
--   go (Add e0 e1) c = "(" ++ go e0 c ++ " + " ++ go e1 c ++ ")"
--   go (Sub e0 e1) c = "(" ++ go e0 c ++ " - " ++ go e1 c ++ ")"
--   go (Mul e0 e1) c = "(" ++ go e0 c ++ " * " ++ go e1 c ++ ")"
--   go (Let e0 e1) c = "(let " ++ v ++ " = " ++ go e0 (c + 1) ++
--                      " in " ++ go (e1 v) (c + 1) ++ ")"
--     where v = "v" ++ show c

tree :: (Num a, Eq a) => a -> Expr
tree 0 = 1
tree n = Let (tree (n - 1)) (\shared -> shared + shared)

autoTree :: (Num b, Eq b, Mode t, Scalar t ~ Expr) => b -> t
autoTree 0 = auto 1
autoTree n = auto $ Let (tree (n - 1)) (\shared -> shared + shared)


