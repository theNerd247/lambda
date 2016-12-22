{-# LANGUAGE DeriveDataTypeable #-}
module Parser where

import Data.Data
import Control.Applicative
import Data.Attoparsec.Text.Lazy
import Control.Monad.Catch
import qualified Data.Text.Lazy as T

data LambdaParserException =
  LambdaParserException String
  deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Exception LambdaParserException

runParser
  :: (MonadThrow m)
  => Parser a -> T.Text ->  m a
runParser p =
  either (throwM . LambdaParserException) return . eitherResult . (parse p) 

runParserString p = runParser p . T.pack
