{-# LANGUAGE TemplateHaskell, TypeApplications #-}
module Main(main) where

import A
import Data.Functor.Identity
import Parsley (_code, WQ)

main = do
  print ($$(_code @WQ test3) 'a')
  print ((runIdentity test4) 'a')
  print (just 'a')
  print (qux 'a')

just :: a -> Maybe a
just = $$(_code @WQ test7)

qux :: a -> a
qux = $$(_code @WQ test5)
