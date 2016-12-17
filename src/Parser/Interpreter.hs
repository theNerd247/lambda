{-# LANGUAGE OverloadedStrings #-}

module Parser.Interpreter where

import Data.Attoparsec.Text
import Data.Interpreter
import Parser.Lambda
import Control.Applicative
import Data.Char (isLower,digitToInt)

token x = skipSpace *> x <* skipSpace

parseCmds = parseAssignCmd <|> parseShowEnv <|> parseEncode <|> parseQuit <|> parseEvalCmd

parseAssignCmd = AssignVar <$> parseVar <* assignSym <*> parseLambda
  where
    assignSym = token $ string ":="

parseEvalCmd = EvalLambda <$> parseLambda

parseShowEnv = pure ShowEnv <* (token $ string "show")

parseEncode = pure EncodeNum <* token (string "enc") <*> parseVar <*> token parseNum

parseNum = toNum <$> many1 digit
  where
    toNum [] = 0
    toNum y@(x:xs) = (digitToInt x)*(10^(length y-1)) + toNum xs

parseQuit = pure Quit <* (token $ string "quit" <|> string ":q")
