# Parsley Garnish ![GitHub release](https://img.shields.io/github/v/release/j-mie6/parsley-garnish) [![GitHub license](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://github.com/j-mie6/parsley-garnish/blob/master/LICENSE) ![GitHub commits since latest release (by SemVer)](https://img.shields.io/github/commits-since/j-mie6/parsley-garnish/latest) [![Hackage Version](https://img.shields.io/hackage/v/parsley-garnish)](https://hackage.haskell.org/package/parsley-garnish) ![Dependent repos (via libraries.io)](https://img.shields.io/librariesio/dependent-repos/hackage/parsley-garnish)
This repo houses the `parsley`-specific GHC plugins to help make writing 
parsers with `parsley` take less boilerplate.

Each plugin here will in some way interact to produce values of either 
type `Parsley.WQ` or `Parsley.Defunctionalized.Defunc`. These datatypes 
are how `parsley`'s API interacts with user-land functions.

By default, the user can produce values of these types by using the 
`makeQ` function:

```hs
makeQ :: Quapplicative q => a -> Code a -> q a
```

Where both `WQ` and `Defunc` are instances of `Quapplicative`. However, 
this can be tedious to do by hand.

## `OverloadedQuotes`
The first plugin found in the garnish hijacks the regular Haskell syntax 
for _Untyped_ Template Haskell (UTH). Since `parsley` uses _Typed_ 
Template Haskell (TTH), it is unlikely that a user of the library will 
need to be using UTH in the same file (with the possible exception of 
top-level splices, or quotes other than the basic `[|x|]`). This plugin 
will transform every UTH quote in a file so that it represents a value of 
`Quapplicative q => q a`. This transformation is as follows:

```hs
qsucc :: Quapplicative q => q Int -> q Int
qsucc qx = [|$(qx) + 1|]
-- goes to:
qsucc qx = makeQ (_val qx + 1) [||$$(_code qx) + 1||]
```

Values of `Defunc` can also be spliced in directly:

```hs
diffcons :: Defunc a -> Defunc ([a] -> [a]) -> Defunc ([a] -> [a])
diffcons qx qdxs = [| $(COMPOSE) ($(CONS) $(qx)) $(qdxs) |]
```

And lambda abstraction works too (along with any other syntax):

```hs
diffcons' :: Defunc (a -> ([a] -> [a]) -> [a] -> [a])
diffcons' = [|\x dxs -> $(diffcons [|x|] [|dxs|])|]
```

The disadvantage to this plugin _currently_ is that it does not make any 
attempt to  leverage the specialised parts of `Defunc` to improve the code 
generation and inspectibility. The user would be left to use this manually.