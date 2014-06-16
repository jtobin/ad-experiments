{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module ParamBasic where

import Control.Applicative
import Data.Graph
import Data.Maybe
import Data.Reify hiding (Graph)
import qualified Data.Reify as Reify
import Data.Reify.Graph.CSE
import Numeric.AD
import System.IO.Unsafe

data Expr a =
    Lit a
  | Var String
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  deriving (Eq, Show)

instance Num a => Num (Expr a) where
  fromInteger = Lit . fromInteger
  e0 + e1     = Add e0 e1
  e0 - e1     = Sub e0 e1
  e0 * e1     = Mul e0 e1

data ExprF a e =
    LitF a
  | AddF e e
  | SubF e e
  | MulF e e
  | VarF String
  deriving (Eq, Ord, Show, Functor)

instance MuRef (Expr a) where
  type DeRef (Expr a)    = ExprF a
  mapDeRef h (Add e0 e1) = AddF <$> h e0 <*> h e1
  mapDeRef h (Sub e0 e1) = SubF <$> h e0 <*> h e1
  mapDeRef h (Mul e0 e1) = MulF <$> h e0 <*> h e1
  mapDeRef _ (Lit v)     = pure (LitF v)
  mapDeRef _ (Var s)     = pure (VarF s)

autoEval :: Mode t => String -> Expr (Scalar t) -> t -> t
autoEval x e0 = (`ec` e0) where
  ec _ (Lit d) = auto d
  ec v (Var y) 
    | x == y    = v
    | otherwise = error "kaboom"
  ec v (Add e1 e2) = ec v e1 + ec v e2
  ec v (Sub e1 e2) = ec v e1 - ec v e2
  ec v (Mul e1 e2) = ec v e1 * ec v e2

toGraphDerivative
  :: (MuRef s, Num s)
  => Expr s -> String -> s -> Reify.Graph (DeRef s)
toGraphDerivative expr x v =
  unsafePerformIO . reifyGraph $ diff (autoEval x expr) v

-- graphEval :: Num a => Expr a -> a
graphEval expr = consume reified where
  reified = unsafePerformIO (toGraph <$> reifyGraph expr)
  toGraph (Reify.Graph env _) = graphFromEdges . map toNode $ env
  toNode (j, AddF a b) = (AddF a b, j, [a, b])
  toNode (j, SubF a b) = (SubF a b, j, [a, b])
  toNode (j, MulF a b) = (MulF a b, j, [a, b])
  toNode (j, LitF d)   = (LitF d, j, [])
  toNode (_, VarF _)   = error "kaboom!"

-- consume :: (Num d, Eq a) => (Graph, Vertex -> (ExprF d a, a, b), c) -> d
consume (g, vmap, _) = go (reverse . topSort $ g) [] where
  go [] acc = snd $ head acc
  go (v:vs) acc =
    let nacc = evalNode (vmap v) acc : acc
    in  go vs nacc

-- need to incorporate the variable name in evalNode
-- evalNode :: (Num d, Eq a) => (ExprF d a, b, c) -> [(a, d)] -> (b, d)
evalNode (LitF d, k, _)   _ = (k, auto d)

evalNode (VarF _, _, _)   _ = error "kaboom!"

evalNode (AddF a b, k, _) l =
  let v = fromJust ((+) <$> lookup a l <*> lookup b l)
  in  (k, v)

evalNode (SubF a b, k, _) l =
  let v = fromJust ((-) <$> lookup a l <*> lookup b l)
  in  (k, v)

evalNode (MulF a b, k, _) l =
  let v = fromJust ((*) <$> lookup a l <*> lookup b l)
  in  (k, v)

-- so, our expression:
test :: Num a => Expr a
test = Var "x" ^ 100

-- examples:
-- toGraphDerivative test "x" (Lit 10)
-- cse $ toGraphDerivative test "x" (Lit 10)

-- evalClose :: Mode t => String -> Expr (Scalar t) -> t -> t
-- evalClose x e0 = (`ec` e0) where
--   ec _ (Lit d) = auto d
--   ec v (Var y) 
--     | x == y    = v
--     | otherwise = error "kaboom"
--   ec v (Add e1 e2) = ec v e1 + ec v e2
--   ec v (Sub e1 e2) = ec v e1 - ec v e2
--   ec v (Mul e1 e2) = ec v e1 * ec v e2

