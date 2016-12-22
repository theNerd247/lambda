{-# LANGUAGE OverloadedStrings #-}

module Parser.Interpreter where

import Data.Attoparsec.Text.Lazy
import Data.Interpreter
import Parser.Lambda
import Control.Applicative
import Data.Char (isLower,digitToInt,isSpace)

token x = skipSpace *> x <* skipSpace

parseScript = many $ token parseCmds

parseCmds = parseAssignCmd <|> parseShowEnv <|> parseEncode <|> parseQuit <|> parseEvalCmd

parseAssignCmd = AssignVar <$> parseVar <* assignSym <*> parseLambda
  where
    assignSym = token $ string ":="

parseEvalCmd = EvalLambda <$> parseLambda

parseShowEnv = pure ShowEnv <* (token $ string "show")

parseEncode = pure EncodeNum <* token (string "enc") <*>  parseNum

parseNum = toNum <$> many1 digit
  where
    toNum [] = 0
    toNum y@(x:xs) = (digitToInt x)*(10^(length y-1)) + toNum xs

parseQuit = pure Quit <* (token $ string "quit" <|> string ":q")
