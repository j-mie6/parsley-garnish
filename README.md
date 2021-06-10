# Parsley Garnish
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

## `LiftPlugin`: [Originally](https://github.com/mpickering/lift-plugin) by *mpickering* (Matthew Pickering)
Automatically lift functions and values using `code`, built as a 
Typechecker Plugin. This is a more lightweight and less invasive plugin
compared with `OverloadedQuotes`. It transforms uses of 
`Parsley.LiftPlugin.code` into `makeQ` calls, allowing the lifting of 
functions and values. The caveat is that it will only work for single 
terms, not function applications, or composite expressions.

```hs
qsucc :: LiftTo q => q (Int -> Int)
qsucc = code succ
-- goes to
qsucc = makeQ succ [||succ||]
```

This plugin is preferable to `OverloadedQuotes` when UTH is needed. It may
also be slightly more efficient as it replaces `Lift` dictionaries as 
opposed to a syntax transformation.

## `OverloadedSyntaxPlugin`: [Originally](https://github.com/mpickering/lift-plugin) by *mpickering* (Matthew Pickering)
By using `overload`, functions can be written to be injected into `WQ` or 
`Defunc` using regular Haskell syntax. This is limited to certain 
constructions. The supported syntax is:

* `if then else`
* `let`
* lambdas
* function application
* pattern matching on `(:)`
* pattern matching on `(,)`

This is a more restricted form of plugin from `OverloadedQuotes` but it
does allow for the use of UTH quotes in the program. It is also capable
of making use of the specialised `Defunc` forms when appropriate, which
improves the inspectibility of code in the `parsley` engine.
