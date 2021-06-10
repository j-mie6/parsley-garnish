{-|
Module      : Parsley.OverloadedQuotesPlugin
Description : Plugin for replacing Untyped Template Haskell quotes with
              @parsley@'s @Quapplicative@ values.
License     : BSD-3-CLAUSE
Maintainer  : Jamie Willis
Stability   : stable

This plugin hijacks the regular Haskell syntax 
for /Untyped/ Template Haskell (UTH). Since @parsley@ uses /Typed/ 
Template Haskell (TTH), it is unlikely that a user of the library will 
need to be using UTH in the same file (with the possible exception of 
top-level splices, or quotes other than the basic @[|x|]@). This plugin 
will transform every UTH quote in a file so that it represents a value of 
@Quapplicative q => q a@. This transformation is as follows:

> qsucc :: Quapplicative q => q Int -> q Int
> qsucc qx = [|$(qx) + 1|]
> -- goes to:
> qsucc qx = makeQ (_val qx + 1) [||$$(_code qx) + 1||]

Values of `Parsley.Defunctionalized.Defunc` can also be spliced in directly:

> diffcons :: Defunc a -> Defunc ([a] -> [a]) -> Defunc ([a] -> [a])
> diffcons qx qdxs = [| $(COMPOSE) ($(CONS) $(qx)) $(qdxs) |]

And lambda abstraction works too (along with any other syntax):

> diffcons' :: Defunc (a -> ([a] -> [a]) -> [a] -> [a])
> diffcons' = [|\x dxs -> $(diffcons [|x|] [|dxs|])|]

The disadvantage to this plugin /currently/ is that it does not make any 
attempt to  leverage the specialised parts of @Defunc@ to improve the code 
generation and inspectibility. The user would be left to use this manually.
-}
module Parsley.OverloadedQuotesPlugin (module Plugin) where

import Parsley.OverloadedQuotesPlugin.Plugin as Plugin
