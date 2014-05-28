{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

-- What needs to be done here is to differentiate a captured function.  This is
-- proving to be difficult because the type has to be Num a => a -> a or
-- [a] -> a.

module Basic where

import Utils
import System.IO.Unsafe

-- | Language expressions.
data Expr = 
    Lit Int
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  deriving (Eq, Ord, Show, Data, Typeable)

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

-- | Pattern functor corresponding to 'Expr'.
data ExprF e =
    LitF Int
  | AddF e e
  | SubF e e
  | MulF e e
  | VarF String
  deriving (Eq, Ord, Show, Functor)

-- | Evaluate expressions.
eval :: Expr -> [(String, Int)] -> Int
eval (Lit d)     _   = d
eval (Add e0 e1) env = eval e0 env + eval e1 env
eval (Sub e0 e1) env = eval e0 env - eval e1 env
eval (Mul e0 e1) env = eval e0 env * eval e1 env
eval (Var v) env     = fromJust $ lookup v env

-- | Tree builder.
--
--   > tree 30
--   1073741824
--
--   > eval (tree 30) []
--   -- don't wait up
--
--   NOTE: can't reify this function using this language.  need something that
--         can observe the loop structure.
--
tree :: (Num a, Eq a, Num b) => a -> b
tree 0 = 1
tree n =
  let shared = tree (n - 1)
  in  shared + shared

-- | Cast an expression to a function.  Assumes one free variable, which must
--   be passed in string form as an argument.
toFunction :: Expr -> String -> (Int -> Int)
toFunction e x = \l -> eval (capture e x l) []

-- | Cast an expression to a function.  All free variables and their
--   replacement values must be held in the environment passed as an argument.
toFunctionWithEnv :: Expr -> ([(String, Int)] -> Int)
toFunctionWithEnv e = \ls -> eval (captureWithEnv e ls) ls 

-- | Capture the body of a function, replacing the named free variable with the
--   provided value.
capture :: Expr -> String -> Int -> Expr
capture e x l = transform replaceVar e where
  replaceVar (Var y) | y == x = Lit l
  replaceVar y = y

-- | Capture the body of a function, replacing any named free variables with
--   their provided values.
captureWithEnv :: Expr -> [(String, Int)] -> Expr
captureWithEnv e ls = transform replaceVar e where
  replaceVar (Var y) = Lit . fromJust . lookup y $ ls
  replaceVar y = y


-- | Simple test expression.
--
--   > g@(Graph env r) <- reifyGraph $ capture test "x" 1
--   > cse g
test :: Expr
test = Var "x" ^ 10

-- | Evaluate a graph that is assumed to include no free variables.
evalGraph :: Graph ExprF -> Int
evalGraph (Graph env r) = go env r where
  go g j = case lookup j g of
    Just (MulF a b) -> (go env a) * (go env b)
    Just (AddF a b) -> (go env a) + (go env b)
    Just (SubF a b) -> (go env a) - (go env b)
    Just (VarF _)   -> error "evalGraph: contains free variable"
    Just (LitF d)   -> d
    Nothing         -> 0

-- | Evaluate a graph by passing an environment containing values for free
--   variables.
evalGraphWithEnv :: Graph ExprF -> [(String, Int)] -> Int
evalGraphWithEnv (Graph env r) e = go env r where
  go g j = case lookup j g of
    Just (MulF a b) -> (go env a) * (go env b)
    Just (AddF a b) -> (go env a) + (go env b)
    Just (SubF a b) -> (go env a) - (go env b)
    Just (VarF v)   -> fromJust $ lookup v e
    Just (LitF d)   -> d
    Nothing         -> 0

main :: IO ()
main = do
  -- presumably we would use unsafePerformIO in practice
  let g0 = unsafePerformIO . reifyGraph $ capture test "x" 2
      g1 = cse g0

  print $ evalGraph g0
  print $ evalGraph g1

