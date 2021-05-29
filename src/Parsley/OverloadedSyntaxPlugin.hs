module Parsley.OverloadedSyntaxPlugin (Syntax(..), overload, module Plugin) where

import Parsley.LiftPlugin (LiftTo)
import Parsley.OverloadedSyntaxPlugin.Plugin as Plugin (plugin)

-- Syntax we can overload
class LiftTo r => Syntax r where
  -- Simple overloading
  _if :: r Bool -> r a -> r a -> r a
  _lam :: (r a -> r b) -> r (a -> b)
  _let :: r a -> (r a -> r b) -> r b
  _ap :: r (a -> b) -> r a -> r b

  -- Case overloading
  _uncons ::  r [a] -> r res -> (r a -> r [a] -> r res) -> r res
  _elim_prod :: r (a, b) -> (r a -> r b -> r x) -> r x

overload :: Syntax r => a -> r a
overload = undefined
{-# NOINLINE overload #-}