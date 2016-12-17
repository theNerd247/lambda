{-# LANGUAGE DeriveDataTypeable #-}
module Parser where

import Data.Data
import Control.Applicative
import Data.Attoparsec.Text
import Control.Monad.Catch
import qualified Data.Text as T

data LambdaParserException =
  LambdaParserException String
  deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Exception LambdaParserException

runParser
  :: (MonadThrow m)
  => Parser a -> String ->  m a
runParser p =
  either (throwM . LambdaParserException) return .
  parseOnly (p <* endOfInput) . T.pack
