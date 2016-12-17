{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Lambda where

import GHC.Generics
import Data.Data
import Data.List
import Control.Monad.Fix (fix)

type VarName = Char

data Lambda
  = LVar VarName
  | Bind VarName
         Lambda
  | App Lambda
        Lambda
  deriving (Eq, Ord, Show, Read, Data, Typeable)

bind :: String -> Lambda -> Lambda
bind [] = id
bind (x:xs) = Bind x . (bind xs)

nApply :: Int -> Lambda -> Lambda -> Lambda
nApply n f x = foldl (flip App) x (replicate n f)

applys :: [Lambda] -> Lambda
applys = foldl1 App

lvars :: [VarName] -> [Lambda]
lvars = fmap LVar

-- | `sub E ('x',N)` substitutes all free variables with name 'x' with expression
-- N.
sub :: Lambda -> (VarName, Lambda) -> Lambda
sub x@(LVar e) (v, n)
  | e == v = n
  | otherwise = x
sub x@(Bind ve e) y@(v, n)
  | ve == v = x
  | otherwise =
    let (newVe,newE) = renameBoundVars x n
    in Bind newVe (sub newE y)
sub (App e1 e2) x = App (sub e1 x) (sub e2 x)

-- | `renameBoundVars (Bind x E) N` renames all occurences of x in E such that the
-- newly chosen name variablename isn't in either variable sets of E or N. This
-- replacement only occures if x `elem` freeVars(N) is true.
renameBoundVars :: Lambda -> Lambda -> (VarName, Lambda)
renameBoundVars (Bind x e) n 
  | x `notElem` (freeVars n) = (x,e)
  | otherwise = (newX, sub e (x,LVar newX))
  where
    newX = head $ (['a'..'z'] \\ (allVars e)) \\ (allVars n)
renameBoundVars _ _ = undefined

freeVars :: Lambda -> [VarName]
freeVars (LVar x) = [x]
freeVars (Bind x e) = freeVars e \\ [x]
freeVars (App e1 e2) = union (freeVars e1) (freeVars e2)

boundVars :: Lambda -> [VarName]
boundVars (LVar x) = []
boundVars (Bind x e) = x : (boundVars e)
boundVars (App e1 e2) = boundVars e1 `union` boundVars e2

allVars e = freeVars e `union` boundVars e

displayLambda :: Lambda -> String
displayLambda (LVar x) = [x]
displayLambda (Bind x e) = "L" ++ [x] ++ "." ++ (displayLambda e)
displayLambda (App e1 e2) = (showL e1) ++ (showL e2)
  where
    showL e@(Bind _ _) = paren e
    showL e@(App _ _) = paren e
    showL e = displayLambda e
    paren e = "(" ++ (displayLambda e) ++ ")"

-- | performs a single beta reduction step on the given lambda term
betaReduct :: Lambda -> Lambda
betaReduct (App (Bind x e1) e2) = sub e1 (x,e2)
betaReduct x@(LVar _) = x
betaReduct (Bind x e) = Bind x (betaReduct e)
betaReduct (App e1 e2) = App (betaReduct e1) (betaReduct e2)

isNormal :: Lambda -> Bool
isNormal (App (Bind _ _) _) = False
isNormal (LVar _) = True
isNormal (Bind _ e) = isNormal e
isNormal (App e1 e2) = isNormal e1 && isNormal e2

betaReduce :: Lambda -> Lambda
betaReduce = fix (\f e -> if (isNormal e) then e else f (betaReduct e))
