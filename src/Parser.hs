{-# LANGUAGE DeriveDataTypeable #-}

module Parser where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (pack)
import Lambda
import Control.Applicative
import Data.Char (isLower)
import Control.Monad.Catch
import Data.Data

data LambdaParserException =
  LambdaParserException String
  deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Exception LambdaParserException

-- | E := x | Lx.E | EE

lambdaParser
  :: (MonadThrow m)
  => String -> m Lambda
lambdaParser =
  either (throwM . LambdaParserException) return .
  parseOnly (parseLambda <* endOfInput) . pack

chainl1 p f = foldl (flip ($)) <$> p <*> many (flip <$> f <*> p)

parseLambda = chainl1 parseL' $ pure App

parseL' =  parens parseLambda <|> parseBind <|> parseLVar
 
parens x = char '(' *> x <* char ')'

parseLVar = LVar <$> parseVar

parseBind = pure Bind <* char 'L' <*> parseVar <* char '.' <*> parseLambda

parseVar = satisfy isLower
