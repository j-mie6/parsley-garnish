{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, RankNTypes #-}
import Data.Generics hiding (Generic)
import GHC.Generics
import Debug.Trace (trace, traceShow)
import Control.Monad.State
import Control.Monad.Reader
import Data.Map (Map)

import qualified Data.Map as Map

type Var = String
data Lit = Int Int | Char Char deriving (Generic, Data)
data Exp = Lam Var Exp | Let Var Exp Exp
         | App Exp Exp | Var Var | Lit Lit
         | UQu Exp | TQu Exp
         | USp Exp | TSp Exp
         deriving (Generic, Data)
data Def = Fun Var [Var] Exp deriving (Generic, Data)
data Top = Top [Def] deriving (Generic, Data)

instance Show Lit where
  show (Int x) = show x
  show (Char c) = show c

instance Show Exp where
  show (Lam v e) = "\\" ++ v ++ " -> " ++ show e
  show (Let v e1 e2) = "let " ++ v ++ " = " ++ show e1 ++ " in " ++ show e2
  show (App e1 e2) = show e1 ++ " " ++ let s2 = show e2 in if elem ' ' s2 then "(" ++ s2 ++ ")" else s2
  show (Var v) = v
  show (UQu e) = "[|" ++ show e ++ "|]"
  show (TQu e) = "[||" ++ show e ++ "||]"
  show (USp e) = "$" ++ let s = show e in if elem ' ' s then "(" ++ s ++ ")" else s
  show (TSp e) = "$$" ++ let s = show e in if elem ' ' s then "(" ++ s ++ ")" else s
  show (Lit n) = show n

instance Show Def where
  show (Fun name args body) = unwords (name : args) ++ " = " ++ show body

instance Show Top where
  show (Top defs) = unlines (map show defs)

_val :: Exp -> Exp
_val = App (Var "_var")

_code :: Exp -> Exp
_code = App (Var "_code")

makeQ :: Exp -> Exp -> Exp
makeQ = App . App (Var "makeQ")

undef = Var "undefined"

makeVal :: Exp -> Exp
makeVal = flip makeQ undef

makeCode :: Exp -> Exp
makeCode = makeQ undef . TQu

prog = Top [
    Fun "plus" [] $ Lam "p" (Lam "q" (App (App (App (Var "liftA2") (UQu (Var "(+)"))) (Var "p")) (Var "q"))),
    Fun "succ" ["x"] $ App (App (Var "(+)") (Var "x")) (Lit (Int 1)),
    Fun "foo" ["f", "x"] $ UQu (App (USp (Var "f")) (USp (Var "x"))),
    Fun "bar" [] $ USp (Var "foo"),
    Fun "baz" [] $ UQu (Lam "x" (USp (App (App (Var "foo") (UQu (Var "succ"))) (UQu (Var "x")))))
  ]

type StageCheck a = ReaderT (Bool, Int) (State (Map Var Int)) a

inQuote :: StageCheck Bool
inQuote = asks fst

level :: StageCheck Int
level = asks snd

withinQuote :: StageCheck a -> StageCheck a
withinQuote = local (\(_, l) -> (True, l + 1))

withinSplice :: StageCheck a -> StageCheck a
withinSplice = local (\(b, l) -> (b, l - 1))

declare :: Var -> StageCheck ()
declare v = do
  n <- level
  modify (Map.insert v n)

wellStaged :: Var -> StageCheck Bool
wellStaged v = gets (maybe True (> 0) . Map.lookup v)

transformUQuote :: Exp -> Exp
transformUQuote q@(UQu x) = trace ("quote to process: " ++ show q ++ "\nlocal bindings: " ++ show (findBindings x)) q
transformUQuote x         = x

-- find all the bindings within a quote
findBindings :: Exp -> [Var]
findBindings = collect whereBinding
  where
    whereBinding :: Exp -> Maybe Var
    whereBinding (Let v _ _) = Just v
    whereBinding (Lam v _)   = Just v
    whereBinding _           = Nothing

collect :: Typeable a => (a -> Maybe r) -> GenericQ [r]
collect f = everything (++) ([] `mkQ` (maybe [] pure . f))

applyPlugin :: Top -> Top
applyPlugin p = {-evalState (runReaderT (-}everywhere' (mkT transformUQuote) p{-}) (False, 0)) Map.empty-}

main :: IO ()
main = do
  print prog
  print (applyPlugin prog)