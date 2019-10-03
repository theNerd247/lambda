{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Lambda where

import GHC.Generics
import Data.Data
import Data.List
import Control.Monad.Fix (fix) 
import Data.Hylo

type VarName = String

data LambdaF a = 
    LVar VarName
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

sub :: VarName -> Lambda -> Lambda -> Lambda
sub v e = cata $ subF v e
  where
    subF v e l@(LVar x)
      | v == x = e
      | otherwise = lvar x
    subF _ _ l = Fix l

beta :: Lambda -> Lambda
beta = cata betaF
  where
    betaF (App l@(Fix (Bind v m)) n) = sub v n m
    betaF l = Fix l
