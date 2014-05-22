
module Utils (
    square
  , sumSquares
  , module Control.Applicative
  , module Data.Reify
  , module Data.Reify.Graph.CSE
  , module Data.Traversable
  , module Numeric.AD.Mode
  , module Numeric.AD.Mode.Kahn
  ) where

import Control.Applicative
import Data.Reify
import Data.Reify.Graph.CSE
import Data.Traversable
import Numeric.AD.Mode
import Numeric.AD.Mode.Kahn

square :: Num a => a -> a
square x = x * x

sumSquares :: Num a => [a] -> a
sumSquares [x, y] = square x + square y

