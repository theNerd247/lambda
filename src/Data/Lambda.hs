{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Lambda where

import GHC.Generics
import Data.Data
import Data.List
import Control.Monad.Fix (fix) 
import Data.Hylo
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid
import Data.String

type VarName = String

-- | TODO: add binding
data LambdaF a = 
    LVar VarName
  | Bind VarName a
  | App a a
  deriving (Eq, Ord, Show, Read, Functor)

type Lambda = Fix LambdaF

instance Show (Fix LambdaF) where
  show = cata displayLambda

instance IsString (LambdaF a) where
  fromString = LVar

instance IsString Lambda where
  fromString = lvar

displayLambda :: LambdaF String -> String
displayLambda (LVar x) = x
displayLambda (Bind x e) = "\\" ++ x ++ "." ++ e
displayLambda (App e1 e2) = "(" ++ e1 ++ ") (" ++ e2 ++ ")"

lvar :: VarName -> Lambda
lvar = Fix . LVar 

lvarchar :: Char -> Lambda
lvarchar = lvar . (:[])

bind :: VarName -> Lambda -> Lambda
bind s = Fix . Bind s

app :: Lambda -> Lambda -> Lambda
app e = Fix . (App e)

(<@>) :: Lambda -> Lambda -> Lambda
(<@>) = app
infixl 4 <@>

withVar :: VarName -> (Lambda -> Lambda) -> Lambda
withVar v f = bind v $ f (lvar v)

(@@) :: VarName -> Lambda -> Lambda
(@@) v l = withVar v (flip app l)
infixr 4 @@

lvars :: [Char] -> [Lambda]
lvars = fmap lvarchar

nApply :: Int -> Lambda -> Lambda -> Lambda
nApply n m = appEndo . mconcat . take n $ repeat (Endo (app m)) 

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

reduce :: Lambda -> Lambda
reduce (Fix (Bind v m)) = bind v . reduce $ beta m
reduce l@(Fix (App (Fix (Bind _ _)) _)) = reduce $ beta l
reduce l = l
