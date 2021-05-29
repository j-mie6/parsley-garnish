module Parsley.LiftPlugin (LiftTo(..), module Plugin) where

import Parsley.LiftPlugin.Plugin as Plugin (plugin)
import Language.Haskell.TH.Syntax (Lift(..))

class LiftTo r where
  code :: Lift a => a -> r a