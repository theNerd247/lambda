module Parser.Lambda where

import Data.Attoparsec.Text.Lazy
import Data.Lambda
import Control.Applicative
import Data.Char (isLower)

assign :: Parser Lambda
assign = do
  name <- varExpr
  manySpace
  ":="
  manySpace
  m <- lambdaExpr
  e <- lambdaExpr
  app (bind name e) m

lambdaExpr :: Parser Lambda
lambdaExpr = lvarExpr <|> bindExpr <|> appExpr

lvarExpr :: Parser Lambda
lvarExpr = lvar <$> varExpr

bindExpr :: Parser Lambda
bindExpr = pure bind <* char '\\' <*> varExpr <* char '.' <*> lambdaExpr

appExpr :: Parser Lambda
appExpr = app <$> (parens lambdaExpr) <*> (parens lambdaExpr)

varExpr :: Parser String
varExpr = many1 $ letter <|> digit

parens :: Parser a -> Parser a
parens x = char '(' *> x <* char ')'
