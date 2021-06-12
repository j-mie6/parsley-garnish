{-|
Module      : Parsley.Internal.Bridge
Description : Stable and package independent names for portions of the @parsley-core@ API
License     : BSD-3-CLAUSE
Maintainer  : Jamie Willis
Stability   : stable

The plugins in this package depend on internals defined in @parsley-core@. This
is fine, but we'd prefer our users to not have to explicitly depend on @parsley-core@
to use the plugins. This module is used to provide stable names for the plugins
to latch onto. It should not be imported by users of the plugins!
-}
module Parsley.Internal.Bridge (_code, _val, makeQ, mkVal, mkCode) where

import qualified Parsley.Internal (Quapplicative(..), Code)

{-|
Re-export of `Parsley.Internal._code`
-}
_code :: Parsley.Internal.Quapplicative q => q a -> Parsley.Internal.Code a
_code = Parsley.Internal._code

{-|
Re-export of `Parsley.Internal._val`
-}
_val :: Parsley.Internal.Quapplicative q => q a -> a
_val  = Parsley.Internal._val

{-|
Re-export of `Parsley.Internal.makeQ`
-}
makeQ :: Parsley.Internal.Quapplicative q => a -> Parsley.Internal.Code a -> q a
makeQ = Parsley.Internal.makeQ

{-|
Produces `Parsley.Internal.Quapplicative` values without code (unsafe!)
-}
mkVal :: Parsley.Internal.Quapplicative q => a -> q a
mkVal x = Parsley.Internal.makeQ x undefined

{-|
Produces `Parsley.Internal.Quapplicative` values without values (unsafe!)
-}
mkCode :: Parsley.Internal.Quapplicative q => Parsley.Internal.Code a -> q a
mkCode qx = Parsley.Internal.makeQ undefined qx