{-# LANGUAGE OverloadedStrings #-}

module Parser.Interpreter where

import Data.Attoparsec.Text
import Data.Interpreter
import Parser.Lambda
import Control.Applicative
import Data.Char (isLower)

token x = skipSpace *> x <* skipSpace

parseCmds = parseAssignCmd <|> parseEvalCmd

parseAssignCmd = AssignVar <$> parseVar <* assignSym <*> parseLambda
  where
    assignSym = token $ string ":="

parseEvalCmd = EvalLambda <$> parseLambda
