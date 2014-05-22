{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module FreePHOAS where

import Utils

data Expr b e =
    Lit Int
  | Var b
  | Add e e
  | Sub e e
  | Mul e e
  | Let e (b -> e)
  deriving (Functor)

let_ :: e -> (Expr b a -> e) -> Expr b e
let_ e0 e1 = Let e0 (\x -> e1 (Var x))

instance Num (Expr b e) where
  fromInteger = Lit . fromInteger
  e0 + e1     = Add e0 e1
  e0 - e1     = Sub e0 e1
  e0 * e1     = Mul e0 e1
  signum      = undefined
  abs         = undefined

--type ClosedExpr = forall a. Expr a
--
--eval :: Expr Int -> Int
--eval (Lit d)     = d
--eval (Var v)     = v
--eval (Add e0 e1) = eval e0 + eval e1
--eval (Sub e0 e1) = eval e0 - eval e1
--eval (Mul e0 e1) = eval e0 * eval e1
--eval (Let e0 e1) = eval (e1 (eval e0))
--
--inline :: Expr (Expr a) -> Expr a
--inline (Lit d)     = Lit d
--inline (Var x)     = x
--inline (Add e0 e1) = Add (inline e0) (inline e1)
--inline (Sub e0 e1) = Sub (inline e0) (inline e1)
--inline (Mul e0 e1) = Mul (inline e0) (inline e1)
--inline (Let e0 e1) = inline (e1 (inline e0))
--
--tree :: (Num a, Eq a) => a -> Expr b
--tree 0 = 1
--tree n = let_ (tree (n - 1)) (\shared -> shared + shared)
--
---- i think i need to do something like this to extract the derivative as a
---- symbolic expression; perhaps one using a simpler syntax.
----
---- in essence, compile this expression type to a graph, then evaluate the graph
--text :: ClosedExpr -> String
--text e = go e 0 where
--  go (Lit j)     _ = show j
--  go (Add e0 e1) c = "(" ++ go e0 c ++ " + " ++ go e1 c ++ ")"
--  go (Sub e0 e1) c = "(" ++ go e0 c ++ " - " ++ go e1 c ++ ")"
--  go (Mul e0 e1) c = "(" ++ go e0 c ++ " * " ++ go e1 c ++ ")"
--  go (Var x) _     = x
--  go (Let e0 e1) c = "(let " ++ v ++ " = " ++ go e0 (c + 1) ++
--                     " in " ++ go (e1 v) (c + 1) ++ ")"
--    where v = "v" ++ show c
--
---- any way to make this Num a => a -> a?
--sharedSquare :: Expr a -> Expr a
--sharedSquare x = let_ x (\shared -> shared * shared)
--
--
