{-# LANGUAGE TemplateHaskell #-}

module Main where

import Parser
import Parser.Interpreter
import Data.Interpreter
import Options.Applicative hiding (runParser)
import Encodes.ChurchNumeral
import Control.Applicative
import Control.Monad.Catch
import Control.Monad
import Control.Monad.State
import Data.Monoid ((<>))
import Control.Lens
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T

data Prog = Prog
  { _progOpts :: ProgOpts
  , _script :: Maybe FilePath
  } deriving (Eq, Ord, Show, Read)

data ProgOpts = ProgOpts
  { _configPath :: FilePath
  } deriving (Eq, Ord, Show, Read)

makeLenses ''Prog
makeLenses ''ProgOpts

programParser =
  info (helper <*> parseProg) $
  fullDesc <> header "lambda - An untyped lambda calculus engine"

parseProg = Prog <$> parseOpts <*> (parseScriptPath <|> pure Nothing)

parseOpts = ProgOpts <$> parseConfigPath

parseConfigPath =
  strOption $
  short 'c' <> long "config" <>
  help
    "global environment config script. This script is run before the passed script" <>
  value ".lambda.ls" <>
  showDefault

parseScriptPath :: Parser (Maybe FilePath)
parseScriptPath =
  fmap Just . strArgument $ metavar "SCRIPT" <> help "the path to the .ls script to run"

parseAndRunScript :: FilePath -> LambdaStateIO ()
parseAndRunScript f = (liftIO $ T.readFile f) >>= runParser parseScript >>= runScript

parseAndRunCmd :: Text -> LambdaStateIO ()
parseAndRunCmd t = runParser parseCmds t >>= runCommand

runProg :: Prog -> LambdaStateIO ()
runProg p = do 
  parseAndRunScript (p ^. progOpts . configPath)
  maybe (forever exeCmd) parseAndRunScript (p ^. script)
  where
    exeCmd =
      (liftIO T.getLine >>= parseAndRunCmd) `catch`
      (\(LambdaParserException s) -> liftIO $ putStrLn s)

main :: IO ()
main =  handle quitExecpt $ execParser programParser >>= flip evalStateT id . runProg
  where
    quitExecpt QuitException = putStrLn "Stopping Lambda Interpreter"
