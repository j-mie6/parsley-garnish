{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{- OPTIONS_GHC -dcore-lint #-}
module A where


import Prelude hiding (Applicative(..))

import Data.Functor.Identity
import Language.Haskell.TH.Syntax

import LiftPlugin

newtype Code a = Code (Q (TExp a))

runCode (Code a) = a

instance LiftTo Code where
  code = Code . unsafeTExpCoerce . lift

instance LiftTo Identity where
  code = Identity


foo 'a' = 'a'

foo1 x = x

test1 :: Code String
test1 = code "1"

test2 :: LiftTo r => r (Char -> Char)
test2 = code foo

test3 :: Code (Char -> Char)
test3 = test2

test4 :: Identity (Char -> Char)
test4 = test2

test5 :: LiftTo r => r (a -> a)
test5 = code foo1

test7 :: LiftTo r => r (a -> Maybe a)
test7 = code Just

{-
- - These should all fail
test8 :: Q Exp
test8 = lift id

l :: (a -> a) -> Q Exp
l = lift

test9 :: Q Exp
test9 = l id
-}

--test6 :: Pure p => (a -> a) -> p (a -> a)
--test6 f = pure f

-- This should fail for now but in theory we should accept it as
-- we would accept `pure return <$> pure ()`.
--test6 :: Pure p => p (IO ())
--test6 = pure (return ())

-- This should fail
--test7 :: String
--test7 = show id

test2_naive = [|| foo ||]

test_con = [|| Just ||]

test_foo1 = [|| foo1 ||]

--test3 = pure foo1

ifTest :: Syntax r => r Bool
ifTest = overload $ if (code True) then (code False) else (code True)

appTest :: Syntax r => r Bool
appTest = overload $ (code const) (code True) (code False)

pureTest :: Syntax r => r (Int)
pureTest = overload $ code (id 5)

lamTest :: Syntax r => r (a -> a)
lamTest = overload $ \a -> a

-- Test for simple var bind
-- This is a bit trickier as can't easier make a lambda as for a normal
-- FunBind
letTest2 :: Syntax r => r Bool
letTest2 = overload $ let t = code True
                     in t

-- Test for fun bind
letTest :: Syntax r => r Bool
letTest = overload $ let t x = x
                     in t (code True)

caseTest :: (Syntax r) => r [a] -> r Bool
caseTest xs = overload $ case xs of
                          [] -> (code False)
                          (_:_) -> (code True)

caseProdTest :: (Syntax r) => r (a, b) -> r a
caseProdTest ab = overload $ case ab of
                               (a, b) -> a

power :: Syntax r => Int -> r (Int -> Int)
power n = let r = power (n - 1)
          in overload $ \k -> if (code (==)) (code n) (code 0)
                              then code 1
                              else (code (*)) k (r k)

staticPower :: Syntax r => r (Int -> Int -> Int)
staticPower = overload (\n -> \k ->
                          if (code (==)) n (code 0)
                                  then code 1
                                  else (code (*)) k (staticPower ((code (-)) n (code 1))  k))

(<*>) :: Syntax r => r (a -> b) -> r a -> r b
(<*>) = _ap

staticPowerId :: Syntax r => r (Int -> Int -> Int)
staticPowerId = overload (\n -> \k ->
                          if ([ n == code 0 ])
                                then code 1
                                else
                                  let sp = staticPowerId <*> ([ n - (code 1) ]) <*> k
                                  in ([ k * sp ]))

staticPower_s :: Syntax r => Int -> r Int -> r Int
staticPower_s n k = if n == 0
                      then code 1
                      else ([ k * (staticPower_s (n - 1) k) ])
