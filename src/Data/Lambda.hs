{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

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
  deriving (Eq, Ord, Show, Read, Data, Typeable)

type Lambda f = Fix LambdaF

class ToVarName a where
  toVarName :: a -> VarName

instance ToVarName Char where
  toVarName = (:[])

instance (ToVarName a) => ToVarName [a] where
  toVarName = mconcat . fmap toVarName

bind :: (ToVarName a) => [a] -> Lambda -> Lambda
bind = foldl ((.Bind . toVarName) . fmap) id

nApply :: Int -> Lambda -> Lambda -> Lambda
nApply n f x = foldl (flip App) x (replicate n f)

applys :: [Lambda] -> Lambda
applys = foldl1 App

lvars :: (ToVarName a) => [a] -> [Lambda]
lvars = fmap (LVar . toVarName)

-- | `sub E ('x',N)` substitutes all free variables with name 'x' with expression
-- N.
sub :: VarName -> LambdaF a -> LambdaF a -> LambdaF a
sub v n x@(LVar e)
  | e == v = n
  | otherwise = x
sub v n (App e1 e2) = App (sub e1 x) (sub e2 x)
sub v n x@(Bind ve e)
  | ve == v = x
  | otherwise =
    let (newVe,newE) = renameBoundVars x n
    in Bind newVe (sub newE y)



-- | `renameBoundVars (Bind x E) N` renames all occurences of x in E such that the
-- newly chosen name variablename isn't in either variable sets of E or N. This
-- replacement only occures if x `elem` freeVars(N) is true.
renameBoundVars :: Lambda -> Lambda -> (VarName, Lambda)
renameBoundVars (Bind x e) n 
  | x `notElem` (freeVars n) = (x,e)
  | otherwise = (newX, sub e (x,LVar newX))
  where
    variableSpace = allVars e `union` allVars n
    newX = getNewVarName x 1
    getNewVarName x n
      | (x++(show n) `elem` variableSpace) = getNewVarName x (n+1)
      | otherwise = x++(show n)
renameBoundVars _ _ = undefined

freeVars :: Lambda -> [VarName]
freeVars (LVar x) = [x]
freeVars (Bind x e) = freeVars e \\ [x]
freeVars (App e1 e2) = (freeVars e1) `union` (freeVars e2)

boundVars :: Lambda -> [VarName]
boundVars (LVar x) = []
boundVars (Bind x e) = x : (boundVars e)
boundVars (App e1 e2) = boundVars e1 `union` boundVars e2

allVars e = freeVars e `union` boundVars e

displayLambda :: Lambda -> String
displayLambda (LVar x) = x
displayLambda (Bind x e) = "\\" ++ x ++ "." ++ (displayLambda e)
displayLambda (App e1 e2) = (showL e1) ++ " " ++ (showR e2)
  where
    showL e@(Bind _ _) = paren e
    showL e = displayLambda e
    showR e@(App _ _) = paren e
    showR e = showL e
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
