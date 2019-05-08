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

lvar :: VarName -> Lambda
lvar = Fix . LVar 

bind :: VarName -> Lambda -> Lambda
bind s = Fix . Bind s

app :: Lambda -> Lambda -> Lambda
app e = Fix . (App e)

data Sub a =
  NoSub Lambda
  | SubBind VarName a
  | SubApp a a
  deriving (Functor)

toLambda :: Alg Sub Lambda
toLambda (NoSub a) = a
toLambda (Sub e) = e

sub :: VarName -> Lambda -> CoAlg LambdaF Lambda
sub v n x@(LVar s))
  | v == s = _
  | otherwise = x
sub v n x@(Bind s e)
  | v == s = x
  | otherwise = SubBind s e
sub v n x@(App (e1 e2)) = App e1 e2

-- | `sub E ('x',N)` substitutes all free variables with name 'x' with expression
-- N.
-- sub :: VarName -> Lambda -> CoAlg LambdaF Lambda
-- sub _ _ x@(App _ _) = Fix x
-- sub v n x@(LVar s)
--   | v == s = n
--   | otherwise = Fix x
-- sub v n x@(Bind s e)
--   | v == s = Fix x
--   | otherwise = n

displayLambda :: LambdaF String -> String
displayLambda (LVar x) = x
displayLambda (Bind x e) = "\\" ++ x ++ "." ++ e
displayLambda (App e1 e2) = "(" ++ e1 ++ ") (" ++ e2 ++ ")"

lambdaId = bind "x" $ lvar "x"

x = app lambdaId $ bind "y" $ lvar "z"
-- y = cata (sub "x" $ bind "x" $ lvar "a") x

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
