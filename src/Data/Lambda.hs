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

data Sub a = 
    NoSub Lambda
  | VarSub Lambda
  | BindSub VarName a
  | AppSub a a
  deriving Functor

mkSub :: VarName -> Lambda -> CoAlg Sub Lambda
mkSub v n = mS . unFix
  where
    mS x@(LVar s)
      | v == s = VarSub n
      | otherwise = NoSub (Fix x)
    mS x@(Bind s e)
      | v == s = NoSub (Fix x)
      | otherwise = BindSub s e
    mS (App e1 e2) = AppSub e1 e2

reSub :: Alg Sub Lambda
reSub (NoSub l) = l
reSub (VarSub l) = l
reSub (BindSub s l) = Fix $ Bind s l
reSub (AppSub e1 e2) = Fix $ App e1 e2

alpha v n = hylo reSub (mkSub v n)

lambdaId = bind "x" $ lvar "x"

x = app (lvar "t") $ bind "y" $ lvar "z"
y = alpha "t" (bind "x" $ lvar "a") x

-- | performs a single beta reduction step on the given lambda term
-- betaReduct :: Lambda -> Lambda
-- betaReduct (App (Bind x e1) e2) = sub e1 (x,e2)
-- betaReduct x@(LVar _) = x
-- betaReduct (Bind x e) = Bind x (betaReduct e)
-- betaReduct (App e1 e2) = App (betaReduct e1) (betaReduct e2)
-- 
-- isNormal :: Lambda -> Bool
-- isNormal (App (Bind _ _) _) = False
-- isNormal (LVar _) = True
-- isNormal (Bind _ e) = isNormal e
-- isNormal (App e1 e2) = isNormal e1 && isNormal e2
-- 
-- betaReduce :: Lambda -> Lambda
-- betaReduce = fix (\f e -> if (isNormal e) then e else f (betaReduct e))
