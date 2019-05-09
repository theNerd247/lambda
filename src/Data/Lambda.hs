{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Lambda where

import GHC.Generics
import Data.Data
import Data.List
import Control.Monad.Fix (fix) 
import Data.Hylo

type VarName = String

data LambdaF a
  = LVar VarName
  | Bind VarName a
  | App a a
  deriving (Eq, Ord, Show, Read, Functor)

type Lambda = Fix LambdaF

instance Show (Fix LambdaF) where
  show = cata displayLambda

displayLambda :: LambdaF String -> String
displayLambda (LVar x) = x
displayLambda (Bind x e) = "\\" ++ x ++ "." ++ e
displayLambda (App e1 e2) = "(" ++ e1 ++ ") (" ++ e2 ++ ")"

lvar :: VarName -> Lambda
lvar = Fix . LVar 

bind :: VarName -> Lambda -> Lambda
bind s = Fix . Bind s

app :: Lambda -> Lambda -> Lambda
app e = Fix . (App e)

data Alpha a = 
    NoAlpha Lambda
  | VarAlpha Lambda
  | BindAlpha VarName a
  | AppAlpha a a
  deriving Functor

mkAlpha :: VarName -> Lambda -> CoAlg Alpha Lambda
mkAlpha v n = mS . unFix
  where
    mS x@(LVar s)
      | v == s = VarAlpha n
      | otherwise = NoAlpha (Fix x)
    mS x@(Bind s e)
      | v == s = NoAlpha (Fix x)
      | otherwise = BindAlpha s e
    mS (App e1 e2) = AppAlpha e1 e2

reAlpha :: Alg Alpha Lambda
reAlpha (NoAlpha l) = l
reAlpha (VarAlpha l) = l
reAlpha (BindAlpha s l) = Fix $ Bind s l
reAlpha (AppAlpha e1 e2) = Fix $ App e1 e2

alpha v n = hylo reAlpha (mkAlpha v n)

data Beta a =
  Beta Lambda
  | BindBeta VarName a
  | AppBeta a a
  deriving (Functor)

mkBeta :: CoAlg Beta Lambda
mkBeta = mB . unFix
  where
    mB (App (Fix (Bind v e1)) e2) = Beta $ alpha v e2 e1
    mB x@(LVar _) = Beta . Fix $ x
    mB (Bind v e) = BindBeta v e
    mB (App e1 e2) = AppBeta e1 e2

reBeta :: Alg Beta Lambda
reBeta (Beta a) = a
reBeta (BindBeta v e) = Fix $ Bind v e
reBeta (AppBeta e1 e2) = Fix $ App e1 e2

beta = hylo reBeta mkBeta

data Reduce a
  NoReduce Lambda
  | ReduceBind VarName a
  | ReduceApp a a
  deriving (Functor)

betaReduce :: CoAlg Reduce Lambda
betaReduce x@(Fix (LVar _)) = NoReduce x
betaReduce x@(App (Fix (Bind v e1)) e2) = Reduce x
betaReduce (Bind v e) = ReduceBind v e
betaReduce (App e1 e2) = ReduceApp e1 e2


x = lvar "x"
y = lvar "y"
true = bind "x" $ bind "y" x
false = bind "x" $ bind "y" y
z = beta $ app true false 
r = app true true
t = app false false
b = app r t
a = beta b
