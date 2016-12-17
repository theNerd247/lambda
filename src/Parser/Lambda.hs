module Parser.Lambda where

import Data.Attoparsec.Text
import Data.Lambda
import Control.Applicative
import Data.Char (isLower)

chainl1 p f = foldl (flip ($)) <$> p <*> many (flip <$> f <*> p)

parseLambda = chainl1 parseL' $ pure App

parseL' =  parens parseLambda <|> parseBind <|> parseLVar
 
parens x = char '(' *> x <* char ')'

parseLVar = LVar <$> parseVar

parseBind = pure Bind <* char '\\' <*> parseVar <* char '.' <*> parseLambda

parseVar = letter
