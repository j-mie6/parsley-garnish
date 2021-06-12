module Parsley.Internal.Bridge (_code, _val, makeQ, mkVal, mkCode) where

import qualified Parsley.Internal.Common.Utils as Parsley (Quapplicative(..), Code)

_code :: Parsley.Quapplicative q => q a -> Parsley.Code a
_code = Parsley._code

_val :: Parsley.Quapplicative q => q a -> a
_val  = Parsley._val

makeQ :: Parsley.Quapplicative q => a -> Parsley.Code a -> q a
makeQ = Parsley.makeQ

mkVal :: Parsley.Quapplicative q => a -> q a
mkVal x = Parsley.makeQ x undefined

mkCode :: Parsley.Quapplicative q => Parsley.Code a -> q a
mkCode qx = Parsley.makeQ undefined qx