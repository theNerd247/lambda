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

lambdaId = bind "x" $ lvar "x"

x = app (lvar "t") $ bind "y" $ lvar "z"
y = alpha "t" (bind "x" $ lvar "a") x

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

-- isNormal :: Lambda -> Bool
-- isNormal (App (Bind _ _) _) = False
-- isNormal (LVar _) = True
-- isNormal (Bind _ e) = isNormal e
-- isNormal (App e1 e2) = isNormal e1 && isNormal e2
-- 
-- betaReduce :: Lambda -> Lambda
-- betaReduce = fix (\f e -> if (isNormal e) then e else f (betaReduct e))
